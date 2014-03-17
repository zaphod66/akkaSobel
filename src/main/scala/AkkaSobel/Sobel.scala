package akkaSobel

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
    if (args.length < 2 || args.length > 3) {
      println("Usage: SobelApp <Image> <noOfWorkers> [<threshold>]")
      return
    }
    
    val filename = args(0)
    val noOfWorkers = args(1).toInt
    
    var tmpThreshold = 75
    if (args.length > 2) {
      tmpThreshold = args(2).toInt
    }
    val threshold = tmpThreshold
    
    println("SobelApp " + filename + " -> " + "sobel_" + filename + " (workers = " + noOfWorkers + " threshold = " + threshold + ")")

    val image = ImageIO.read(new File(filename))
    println("Image: " + image.getWidth() + " * " + image.getHeight())

    val config = Configuration(image, noOfWorkers, threshold, filename)
    
    calc(config)
  }
  
  def calc(config: Configuration) {
    val grayImage   = getGrayScale(config.image)
      
    val system = ActorSystem("SobelSystem")
    
    val master = system.actorOf(Props(new Master(grayImage, config.noOfWorkers, config.threshold, config.filename)), name = "master")

    println("Master Start")
    master ! Start
  }
  
  class Master(val srcImage: BufferedImage, val nrOfWorkers: Int, threshold: Int, fileName: String) extends Actor {
    var noOfLines: Int = _
    var sobelDone: Boolean = _
    val start: Long = System.currentTimeMillis
    val height = srcImage.getHeight
    val width  = srcImage.getWidth
    
    val writer = context.actorOf(Props(new ImageWriter(start)), name = "imageWriter")

    writer ! Write(srcImage, "gray_" + fileName, 1)

    val tmpImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val resImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_BYTE_GRAY)

    val sobelRouter = context.actorOf(Props(new SobelOp(srcImage)).withRouter(RoundRobinRouter(nrOfWorkers)), name = "sobelRouter")
    val thresRouter = context.actorOf(Props(new ThresholdOp(tmpImage, threshold)).withRouter(RoundRobinRouter(nrOfWorkers)), name = "thresRouter")

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
            println("Thresholding after " + (System.currentTimeMillis - start).millis)
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
