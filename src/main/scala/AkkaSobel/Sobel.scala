package akkaSobel

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.language.postfixOps

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color

sealed trait OpMessage
case object Start extends OpMessage
case object Stop  extends OpMessage
case class Ops(ops: List[akka.actor.ActorRef])
case class Work(lineNo: Int) extends OpMessage
case class Result(lineNo: Int, lineResult: Array[Int]) extends OpMessage
case class Write(image: BufferedImage, fileName: String, id: Int) extends OpMessage
case class WriteFinished(id: Int) extends OpMessage

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
    
    println("SobelApp " + filename + " -> " + "end_" + filename + " (workers = " + noOfWorkers + " threshold = " + threshold + ")")

    val image = ImageIO.read(new File(filename))
    println("Image: " + image.getWidth() + " * " + image.getHeight())

    val config = Configuration(image, noOfWorkers, threshold, filename)
    
    calc(config)
  }
  
  def calc(config: Configuration) {
    val system = ActorSystem("SobelSystem")
    
    val master = system.actorOf(Props(new Master(config)), name = "master")

    master ! Start
  }
  
  class Master(val config: Configuration) extends Actor {
    
    val srcImage    = config.image
    val noOfWorkers = config.noOfWorkers
    val threshold   = config.threshold
    val fileName    = config.filename
    
    var noOfLines: Int = _
    val start: Long = System.currentTimeMillis
    val height = srcImage.getHeight
    val width  = srcImage.getWidth
    
    val FINISH_ID = 0

    val embossKernel  = Array(Array(-1.0, -0.5,  0.0),
                              Array(-0.5,  1.0,  0.5),
                              Array( 0.0,  0.5,  1.0))
    
    val embossKernel2 = Array(Array(-2.0, -1.5, -1.0,  0.0,  0.0),
                              Array(-1.5, -1.0, -0.5,  0.0,  0.0),
                              Array(-1.0, -0.5,  1.0,  0.5,  1.0),
                              Array( 0.0,  0.0,  0.5,  1.0,  1.5),
                              Array( 0.0,  0.0,  1.0,  1.5,  2.0))
    
    val embossKernel3 = Array(Array(-3.0, -2.5, -2.0, -1.5,  0.0,  0.0,  0.0),
                              Array(-2.5, -2.0, -1.5, -1.0,  0.0,  0.0,  0.0),
                              Array(-2.0, -1.5, -1.0, -0.5,  0.0,  0.0,  0.0),
                              Array(-1.5, -1.0, -0.5,  1.0,  0.5,  1.0,  1.5),
                              Array( 0.0,  0.0,  0.0,  0.5,  1.0,  1.5,  2.0),
                              Array( 0.0,  0.0,  0.0,  1.0,  1.5,  2.0,  2.5),
                              Array( 0.0,  0.0,  0.0,  1.5,  2.0,  2.5,  3.0))
    
    val sharpenKernel = Array(Array(-1.0, -1.0, -1.0),
                              Array(-1.0,  9.0, -1.0),
                              Array(-1.0, -1.0, -1.0))
    
    val tmpImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB)

    val writer = context.actorOf(Props(new ImageWriter(start)), name = "imageWriter")

    val sobelRouter    = context.actorOf(Props(new SobelOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sobelRouter")
    val grayRouter     = context.actorOf(Props(new GrayOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "grayRouter")
    val thresRouter    = context.actorOf(Props(new ThresholdOp(srcImage, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "thresRouter")
    val invertRouter   = context.actorOf(Props(new InvertOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "invertRouter")
    val dilate1Router  = context.actorOf(Props(new DilateOp(srcImage, 1, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilate1Router")
    val dilate2Router  = context.actorOf(Props(new DilateOp(srcImage, 2, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilate2Router")
    val dilate3Router  = context.actorOf(Props(new DilateOp(srcImage, 3, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilate3Router")
    val sharpenRouter  = context.actorOf(Props(new SharpenOp(srcImage, 0.1)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sharpenRouter")
    val blurRouter     = context.actorOf(Props(new BlurOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "blurRouter")
    val embossRouter   = context.actorOf(Props(new ConvolveOp(srcImage, embossKernel2)).withRouter(RoundRobinRouter(noOfWorkers)), name = "embossRouter")
    val sharpOpRouter  = context.actorOf(Props(new ConvolveOp(srcImage, sharpenKernel)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sharpOpRouter")
    val noOpRouter     = context.actorOf(Props(new NoOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "noOpRouter")
    
    var ops = List(blurRouter, sobelRouter, grayRouter, thresRouter, dilate1Router, invertRouter, dilate3Router, dilate1Router)
    
    override def receive = {
      case Start => {
        println("Master Start  after " + (System.currentTimeMillis - start).millis)
        val router = ops.head
        ops = ops.tail
        for (i <- 0 until height) router ! Work(i)
      }
      
      case Stop => {
          println("Master Finish after " + (System.currentTimeMillis - start).millis)
          context.system.shutdown
      }
      
      case Result(lineNo, lineResult) => {
        noOfLines += 1
        
        for (x <- 0 until width) yield {
          val rgb = lineResult(x)
          tmpImage.setRGB(x, lineNo, rgb)
        }
        
        if (noOfLines == height) {
          if (ops.isEmpty) {
            println("All steps done after " + (System.currentTimeMillis - start).millis)
            writer ! Write(tmpImage, "end_" + fileName, FINISH_ID)
          } else {
            noOfLines = 0
            println("Step done after " + (System.currentTimeMillis - start).millis)

            val graphics = srcImage.getGraphics
            graphics.drawImage(tmpImage, 0, 0, null)
            graphics.dispose

            val router = ops.head
            ops = ops.tail
            for (i <- 0 until height) router ! Work(i)
          }
        }
      }
      
      case WriteFinished(id) => {
        if (id == FINISH_ID) {
          self ! Stop
        }
      }
    }
  }
  
  class ImageWriter(val startTime: Long) extends Actor {
    override def receive = {
      case Write(image, fileName, id) => {
        println("Write " + id + " (" + fileName + ") started  after " + (System.currentTimeMillis - startTime).millis)
        ImageIO.write(image, "PNG", new File(fileName))
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
