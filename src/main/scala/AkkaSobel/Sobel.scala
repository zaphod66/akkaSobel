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
    if (args.length != 1) {
      println("Usage: SobelApp <Image>")
      exit(-1)
    }
    
    val filename = args(0)
    
    println("SobelApp " + filename + " -> " + "sobel_" + filename)

    val image = ImageIO.read(new File(filename))
    println("Image: " + image.getWidth() + " * " + image.getHeight())
    
    calc(image, 8, filename)
  }
  
  def calc(image: BufferedImage, nrOfWorkers: Int, fileName: String) {
    val grayImage   = getGrayScale(image)
    val resultImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
      
    val system = ActorSystem("SobelSystem")
    
    val master = system.actorOf(Props(new Master(grayImage, resultImage, nrOfWorkers, fileName)), name = "master")

    println("Start")
    master ! Start
  }
  
  class Master(val srcImage: BufferedImage, val dstImage: BufferedImage, val nrOfWorkers: Int, fileName: String) extends Actor {
    var noOfLines: Int = _
    val start: Long = System.currentTimeMillis
    val height = srcImage.getHeight
    val width  = srcImage.getWidth
    
    val writer = context.actorOf(Props[ImageWriter], name = "imageWriter")

    writer ! Write(srcImage, "gray_" + fileName, 1)
    
    val workerRouter = context.actorOf(Props(new ImageWorker(srcImage)).withRouter(RoundRobinRouter(nrOfWorkers)), name = "WorkerRouter")

    override def receive = {
      case Start => {
        for (i <- 0 until height) workerRouter ! Work(i)
      }
      
      case Result(lineNo, lineResult) => {
        noOfLines += 1
        for (x <- 0 until width) yield {
          val c = lineResult(x).toFloat
          val color = new Color(c, c, c)
          dstImage.setRGB(x, lineNo, color.getRGB())
        }
        
        if (noOfLines == height) {
          println("Writing Result after " + (System.currentTimeMillis - start).millis)
          writer ! Write(dstImage, "sobel_" + fileName, 2)
        }
      }
      
      case WriteFinished(id) => {
        println("Write " + id + " finished after " + (System.currentTimeMillis - start).millis)
        if (id == 2) {
          println("System shutdown")
          context.system.shutdown
        }
      }
    }
  }
  
  class ImageWorker(image: BufferedImage) extends Actor {
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
        val value = Math.min(1.0, Math.sqrt(Math.pow(xValue, 2) + Math.pow(yValue, 2))).toFloat
        
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
  
  class ImageWriter extends Actor {
    override def receive = {
      case Write(image, fileName, id) => {
        ImageIO.write(image, "png", new File(fileName))
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
