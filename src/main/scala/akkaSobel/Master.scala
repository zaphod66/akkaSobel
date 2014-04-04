package akkaSobel

import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor._
import akka.routing.RoundRobinRouter

import java.awt.image.BufferedImage

abstract class OpMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends Actor {

  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  def router: ActorRef
  def name: String
  
  override def receive = {
    case Start => {
      println(s"$name step start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      for (i <- 0 until height) router ! Work(i)
    }
    
    case Result(lineNo, lineResult) => {
      noOfLines += 1

      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }

      if (noOfLines == height) {
        println(s"$name step done  after " + (System.currentTimeMillis - start).millis)
        
        orgSender ! MasterFinish
      }
    }
  }
}

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

class DilateMaster(srcImage: BufferedImage, dstImage: BufferedImage, radius: Int, threshold: Int, noOfWorkers: Int) extends Actor {

  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  val router = context.actorOf(Props(new DilateOp(srcImage, radius, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilateRouter")
  
  override def receive = {
    case Start => {
      println("Dilate step start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      for (i <- 0 until height) router ! Work(i)
    }
    
    case Result(lineNo, lineResult) => {
      noOfLines += 1

      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }

      if (noOfLines == height) {
        println("Dilate step done  after " + (System.currentTimeMillis - start).millis)
        
        orgSender ! MasterFinish
      }
    }
  }  
}

class MedianMaster(srcImage: BufferedImage, dstImage: BufferedImage, radius: Int, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {
  override val name = "Median"
    
  override val router = context.actorOf(Props(new MedianOp(srcImage, radius)).withRouter(RoundRobinRouter(noOfWorkers)), name = "medianRouter")
}

class BlurMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {
  override val name = "Blur"

  override val router = context.actorOf(Props(new BlurOp(srcImage)).withRouter(RoundRobinRouter(noOfWorkers)), name = "blurRouter")
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
