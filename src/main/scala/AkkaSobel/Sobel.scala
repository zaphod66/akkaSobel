package AkkaSobel

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.language.postfixOps

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color

sealed trait SobelMessage
case object Start extends SobelMessage
case class Work(lineNo: Int) extends SobelMessage
case class Result(lineNo: Int, lineResult: Array[Double]) extends SobelMessage
case class Write(image: BufferedImage, fileName: String, id: Int) extends SobelMessage
case class WriteFinished(id: Int) extends SobelMessage

object Sobel extends App {
    
  override def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: SobelApp <Image> <noOfWorkers>")
      exit(-1)
    }
    
    val filename = args(0)
    val noOfWorkers = args(1).toInt
    
    println("SobelApp " + filename + " -> " + "sobel_" + filename + " (workers = " + noOfWorkers + ")")

    val image = ImageIO.read(new File(filename))
    println("Image: " + image.getWidth() + " * " + image.getHeight())
    
    calc(image, noOfWorkers, filename)
  }
  
  def calc(image: BufferedImage, nrOfWorkers: Int, fileName: String) {
    val grayImage   = getGrayScale(image)
      
    val system = ActorSystem("SobelSystem")
    
    val master = system.actorOf(Props(new Master(grayImage, nrOfWorkers, fileName)), name = "master")

    println("Master Start")
    master ! Start
  }
  
  class Master(val srcImage: BufferedImage, val nrOfWorkers: Int, fileName: String) extends Actor {
    var noOfLines: Int = _
    var sobelDone: Boolean = _
    val start: Long = System.currentTimeMillis
    val height = srcImage.getHeight
    val width  = srcImage.getWidth
    
    val writer = context.actorOf(Props(new ImageWriter(start)), name = "imageWriter")

    writer ! Write(srcImage, "gray_" + fileName, 1)

    val tmpImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val resImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_BYTE_GRAY)

    val sobelRouter = context.actorOf(Props(new SobelWorker(srcImage)).withRouter(RoundRobinRouter(nrOfWorkers)), name = "sobelRouter")
    val thresRouter = context.actorOf(Props(new ThresholdWorker(tmpImage, 75)).withRouter(RoundRobinRouter(nrOfWorkers)), name = "thresRouter")

    override def receive = {
      case Start => {
        println("Master Start  after " + (System.currentTimeMillis - start).millis)
        sobelDone = false
        for (i <- 0 until height) sobelRouter ! Work(i)
      }
      
      case Result(lineNo, lineResult) => {
        noOfLines += 1
        for (x <- 0 until width) yield {
          val c = lineResult(x).toFloat
          val color = new Color(c, c, c)
          if (sobelDone) {
            resImage.setRGB(x, lineNo, color.getRGB())
          } else {
            tmpImage.setRGB(x, lineNo, color.getRGB())
          }
        }
        
        if (noOfLines == height) {
          if (sobelDone) {
            writer ! Write(resImage, "sobel_" + fileName, 2)
          } else {
            sobelDone = true
            noOfLines = 0
            println("thresholding after " + (System.currentTimeMillis - start).millis)
            for (i <- 0 until height) thresRouter ! Work(i)
          }
        }
      }
      
      case WriteFinished(id) => {
        if (id == 2) {
          println("Master Finish after " + (System.currentTimeMillis - start).millis)
          context.system.shutdown
        }
      }
    }
  }
  
  class ThresholdWorker(image: BufferedImage, val threshold: Int) extends Actor {
    val width = image.getWidth
    
    override def receive = {
      case Work(lineNo) => {
        val lineResult = calcResult(lineNo)
        sender ! Result(lineNo, lineResult)
      }
    }
    
    private def calcResult(lineNo: Int): Array[Double] = {
      var lineResult = new Array[Double](width)
      
      for (x <- 0 until width) {
        val color = new Color(image.getRGB(x, lineNo))
        
        if (color.getRed() > threshold) {
          lineResult(x) = 1.0
        } else {
          lineResult(x) = 0.0
        }
      }
      
      lineResult
    }
  }
  
  class SobelWorker(image: BufferedImage) extends Actor {
    val width = image.getWidth
    
    val Gy = Array(Array(-1, -2, -1), Array(0, 0, 0), Array(1, 2, 1))
    val Gx = Array(Array(-1, 0, 1), Array(-2, 0, 2), Array(-1, 0, 1))

    override def receive = {
      case Work(lineNo) => {
        val lineResult = calcResult(lineNo)
        sender ! Result(lineNo, lineResult)
      }
    }
    
    private def calcResult(lineNo: Int): Array[Double] = {
      var lineResult = new Array[Double](width)
      
      for (x <- 0 until width) {
        val xValue = mapImagePoint(image, x, lineNo, Gx)
        val yValue = mapImagePoint(image, x, lineNo, Gy)
        val tmp = Math.sqrt(Math.pow(xValue, 2) + Math.pow(yValue, 2))
//        val tmp = Math.max(Math.abs(xValue), Math.abs(yValue))
//        val tmp = Math.abs(yValue)
        val value = Math.min(1.0, tmp).toFloat
        
        lineResult(x) = value
      }
      
      lineResult
    }
    
    private def mapImagePoint(image: BufferedImage, x: Int, y: Int, matrix: Array[Array[Int]]): Double = {
      var result = 0.0
      for {
        dY <- -1 to 1
        dX <- -1 to 1
        cY = y + dY
        cX = x + dX
      } {
        if (cY >= 0 && cY < image.getHeight && cX >= 0 && cX < image.getWidth) {
          val coefficient = matrix(dY + 1)(dX + 1)
          val color = new Color(image.getRGB(cX, cY))
          result += color.getRed.toDouble / 255.0 * coefficient
        }
      }

      result
    }
  }
  
  class ImageWriter(val startTime: Long) extends Actor {
    override def receive = {
      case Write(image, fileName, id) => {
        println("Write " + id + " started  after " + (System.currentTimeMillis - startTime).millis)
        ImageIO.write(image, "png", new File(fileName))
        println("Write " + id + " finished after " + (System.currentTimeMillis - startTime).millis)
        sender ! WriteFinished(id)
      }
    }
  }
  
  // --------------------------------------------------------------------
  
  private def getGrayScale(inputImage: BufferedImage): BufferedImage = {
    val img = new BufferedImage(inputImage.getWidth, inputImage.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val graphics = img.getGraphics
    graphics.drawImage(inputImage, 0, 0, null)
    graphics.dispose()
    img
  }  
}
