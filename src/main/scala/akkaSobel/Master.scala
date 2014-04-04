package akkaSobel

import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor._
import akka.routing.RoundRobinRouter

import java.awt.image.BufferedImage

class SobelMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends Actor {

  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  val sobelRouter = context.actorOf(Props(new SobelOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sobelRouter")
  
  override def receive = {
    case Start => {
      println("Sobel step start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      for (i <- 0 until height) sobelRouter ! Work(i)
    }
    
    case Result(lineNo, lineResult) => {
      noOfLines += 1

      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }

      if (noOfLines == height) {
        println("Sobel step done  after " + (System.currentTimeMillis - start).millis)
        
        orgSender ! MasterFinish
      }
    }
  }
}

class GrayMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends Actor {

  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  val grayRouter = context.actorOf(Props(new GrayOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "grayRouter")
  
  override def receive = {
    case Start => {
      println("Gray step start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      for (i <- 0 until height) grayRouter ! Work(i)
    }
    
    case Result(lineNo, lineResult) => {
      noOfLines += 1

      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }

      if (noOfLines == height) {
        println("Gray step done  after " + (System.currentTimeMillis - start).millis)
        
        orgSender ! MasterFinish
      }
    }
  }
}

class ThresholdMaster(srcImage: BufferedImage, dstImage: BufferedImage, threshold: Int, noOfWorkers: Int) extends Actor {

  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  val thresRouter = context.actorOf(Props(new ThresholdOp(srcImage, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "thresRouter")
  
  override def receive = {
    case Start => {
      println("Threshold step start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      for (i <- 0 until height) thresRouter ! Work(i)
    }
    
    case Result(lineNo, lineResult) => {
      noOfLines += 1

      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }

      if (noOfLines == height) {
        println("Threshold step done  after " + (System.currentTimeMillis - start).millis)
        
        orgSender ! MasterFinish
      }
    }
  }  
}

class InvertMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends Actor {

  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  val thresRouter = context.actorOf(Props(new InvertOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "invertRouter")
  
  override def receive = {
    case Start => {
      println("Invert step start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      for (i <- 0 until height) thresRouter ! Work(i)
    }
    
    case Result(lineNo, lineResult) => {
      noOfLines += 1

      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }

      if (noOfLines == height) {
        println("Invert step done  after " + (System.currentTimeMillis - start).millis)
        
        orgSender ! MasterFinish
      }
    }
  }  
}

class NoOpMaster(srcImage: BufferedImage, dstImage: BufferedImage) extends Actor {

  val start: Long = System.currentTimeMillis
  
  override def receive = {
    case Start => {
      println("NoOp step start after " + (System.currentTimeMillis - start).millis)

      val g = dstImage.getGraphics()
      g.drawImage(srcImage, 0, 0, null)
      g.dispose
      
      println("NoOp step done  after " + (System.currentTimeMillis - start).millis)

      sender ! MasterFinish
    }
  }
}
