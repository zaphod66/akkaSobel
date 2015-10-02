package akkaSobel

import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor._
import akka.routing.{RoundRobinPool}

import java.awt.image.BufferedImage
import java.awt.Color

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

class SobelMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  val name = "Sobel"
  val router = context.actorOf(Props(new SobelOp(srcImage)).withRouter(RoundRobinPool(noOfWorkers)), name = "sobelRouter")

}

class GrayMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  val name = "Gray"
  val router = context.actorOf(Props(new GrayOp(srcImage)).withRouter(RoundRobinPool(noOfWorkers)), name = "grayRouter")

}

class ThresholdMaster(srcImage: BufferedImage, dstImage: BufferedImage, threshold: Int, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  override val name = "Threshold"
  override val router = context.actorOf(Props(new ThresholdOp(srcImage, threshold)).withRouter(RoundRobinPool(noOfWorkers)), name = "thresRouter")

}

class InvertMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  override val name   = "Invert"
  override val router = context.actorOf(Props(new InvertOp(srcImage)).withRouter(RoundRobinPool(noOfWorkers)), name = "invertRouter")
  
}

class DilateMaster(srcImage: BufferedImage, dstImage: BufferedImage, radius: Int, threshold: Int, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  override val name   = "Dilate"
  override val router = context.actorOf(Props(new DilateOp(srcImage, radius, threshold)).withRouter(RoundRobinPool(noOfWorkers)), name = "dilateRouter")

}

class MedianMaster(srcImage: BufferedImage, dstImage: BufferedImage, radius: Int, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  override val name = "Median"  
  override val router = context.actorOf(Props(new MedianOp(srcImage, radius)).withRouter(RoundRobinPool(noOfWorkers)), name = "medianRouter")

}

class BlurMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int) extends OpMaster(srcImage, dstImage, noOfWorkers) {

  override val name   = "Blur"
  override val router = context.actorOf(Props(new BlurOp(srcImage)).withRouter(RoundRobinPool(noOfWorkers)), name = "blurRouter")

}

class SharpenMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int, c: Double) extends OpMaster(srcImage, dstImage, noOfWorkers)
{
  override val name = "Sharpen"
  override val router = context.actorOf(Props(new SharpenOp(srcImage, c)).withRouter(RoundRobinPool(noOfWorkers)), name = "sharpenRouter")
}

class ConvolveMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int, kernel: Array[Array[Double]], conName: String) extends OpMaster(srcImage, dstImage, noOfWorkers) {
  override val name = conName
  override val router = context.actorOf(Props(new ConvolveOp(srcImage, kernel)).withRouter(RoundRobinPool(noOfWorkers)), name = conName + "Router")
}

// ===============================================================================

class CannyMaster(srcImage: BufferedImage, dstImage: BufferedImage, noOfWorkers: Int, upperThres: Double, lowerThres: Double) extends Actor
{
  case class GradientResult(lineNo: Int, gradResult: Array[(Double,Int)])
  case class SupressResult(lineNo: Int, lineResult: Array[Int])
  
  class GradientOp extends Actor
  {
    val Gy = Array(Array(-1.0, -2.0, -1.0), Array( 0.0, 0.0, 0.0), Array( 1.0, 2.0, 1.0))
    val Gx = Array(Array(-1.0,  0.0,  1.0), Array(-2.0, 0.0, 2.0), Array(-1.0, 0.0, 1.0))

    val thetaQ = Math.PI / 4.0
    
    override def receive = {
      case Work(lineNo) => {
        val gradResult = new Array[(Double,Int)](width)
        
        for (x <- 0 until width) {
          gradResult(x) = calcGradient(x, lineNo)
        }
        
        sender ! GradientResult(lineNo, gradResult)
      }
    }
    
    private def calcGradient(x: Int, y: Int): (Double, Int) = {
      val xValue = convolvePoint(srcImage, x, y, Gx)
      val yValue = convolvePoint(srcImage, x, y, Gy)
    
      val xGrad  = ( xValue._1 + xValue._2 + xValue._3 ) / 3.0
      val yGrad  = ( yValue._1 + yValue._2 + yValue._3 ) / 3.0

      val grad   = Math.sqrt(xGrad * xGrad + yGrad * yGrad)
    
      val theta  = Math.atan2(yGrad, xGrad)   // direction of gradient
      val dir    = ( Math.round( theta.toFloat / thetaQ.toFloat ) + 4 ) % 4 // 0 -> 0, 45 -> 1, 90 -> 2, 135 -> 3, 180 -> 3, ...

      (grad, dir)
    }
    
    private def convolvePoint(image: BufferedImage, x: Int, y: Int, kernel: Array[Array[Double]]): (Double, Double, Double) = {
    var result = (0.0, 0.0, 0.0)
    
    val sy = kernel.length
    val sx = kernel(0).length
    
    val by = ( sy - 1 ) / 2
    val bx = ( sx - 1 ) / 2
    
    for {
      dY <- -by to by
      dX <- -bx to bx
      cY = y + dY
      cX = x + dX
    } {
      if (cY >= 0 && cY < image.getHeight && cX >= 0 && cX < image.getWidth) {
        val coefficient = kernel(dY + by)(dX + bx)
        val color = new Color(image.getRGB(cX, cY))
        var r = result._1
        var g = result._2
        var b = result._3
        r += color.getRed.toDouble   / 255.0 * coefficient
        g += color.getGreen.toDouble / 255.0 * coefficient
        b += color.getBlue.toDouble  / 255.0 * coefficient
        result = (r, g, b)
      }
    }

    result
    }
  }
  
  class SuppressOp extends Actor
  {
    override def receive = {
      case Work(lineNo) => {
        val lineResult = new Array[Int](width)

        for (x <- 0 until width) {
//        println(s"SuppressOp: lineNo = $lineNo, x = $x, gradient($lineNo, $x) = " + gradient(lineNo)(x))
        
          val g0 = gradient(lineNo)(x)
        
          val n  = neighbors(g0._2, x, lineNo)

          val g1 = gradient(n._1._2)(n._1._1)
          val g2 = gradient(n._2._2)(n._2._1)

//        val color = if (g0._1 > upperThres) Color.WHITE.getRGB else if (g0._1 > lowerThres) Color.DARK_GRAY.getRGB else Color.BLACK.getRGB
          val color = if (g0._1 > upperThres) Color.WHITE.getRGB else if (g0._1 > lowerThres) Color.RED.getRGB else Color.BLACK.getRGB
          val value = if ((g0._1 >= g1._1) && (g0._1 >= g2._1)) { color } else { Color.BLACK.getRGB }

          lineResult(x) = value
        }

        sender ! SupressResult(lineNo, lineResult)
      }
    }

    private def neighbors(dir: Int, x: Int, y: Int): ((Int, Int), (Int, Int)) = {
      var tmp = dir match {
        case 0 => ((x - 1, y + 0), (x + 1, y + 0))
        case 1 => ((x - 1, y - 1), (x + 1, y + 1))
        case 2 => ((x + 0, y - 1), (x + 0, y + 1))
        case 3 => ((x - 1, y + 1), (x + 1, y - 1))
      }

      if (tmp._1._1 < 0) tmp = ((        0, tmp._1._2), (tmp._2._1, tmp._2._2))
      if (tmp._1._2 < 0) tmp = ((tmp._1._1,         0), (tmp._2._1, tmp._2._2))
      if (tmp._2._1 < 0) tmp = ((tmp._1._1, tmp._1._2), (        0, tmp._2._2))
      if (tmp._2._2 < 0) tmp = ((tmp._1._1, tmp._1._2), (tmp._2._1,         0))

      if (tmp._1._1 >= width)  tmp = ((  width-1, tmp._1._2), (tmp._2._1, tmp._2._2))
      if (tmp._1._2 >= height) tmp = ((tmp._1._1,  height-1), (tmp._2._1, tmp._2._2))
      if (tmp._2._1 >= width)  tmp = ((tmp._1._1, tmp._1._2), (  width-1, tmp._2._2))
      if (tmp._2._2 >= height) tmp = ((tmp._1._1, tmp._1._2), (tmp._2._1,  height-1))

      tmp
    }
  }
  
  var noOfLines: Int = _
  var orgSender: ActorRef = _
  
  val start: Long = System.currentTimeMillis
  val height = srcImage.getHeight
  val width  = srcImage.getWidth

  val gradient = Array.ofDim[(Double, Int)](height, width)  // gradients (magnitude, direction)
  
  val gradRouter = context.actorOf(Props(new GradientOp).withRouter(RoundRobinPool(noOfWorkers)), name = "cannyGradRouter")
  val suppRouter = context.actorOf(Props(new SuppressOp).withRouter(RoundRobinPool(noOfWorkers)), name = "cannySuppRouter")
  
  override def receive = {
    case Start => {
      println("Canny step 1 start after " + (System.currentTimeMillis - start).millis)
      noOfLines = 0
      orgSender = sender
      
      for (i <- 0 until height) gradRouter ! Work(i)
    }
    
    case GradientResult(lineNo, gradResult) => {
      noOfLines += 1

      gradient(lineNo) = gradResult

      if (noOfLines == height) {
        println("Canny step 1 done  after " + (System.currentTimeMillis - start).millis)
        noOfLines = 0

        for (i <- 0 until height) suppRouter ! Work(i)
      }      
    }
    
    case SupressResult(lineNo, lineResult) => {
      noOfLines += 1
      
      for (x <- 0 until width) yield {
        val rgb = lineResult(x)
        dstImage.setRGB(x, lineNo, rgb)
      }
      
      if (noOfLines == height) {
        println("Canny step 2 done  after " + (System.currentTimeMillis - start).millis)
        
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
