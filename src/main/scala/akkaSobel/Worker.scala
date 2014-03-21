package akkaSobel

import akka.actor._

import java.awt.image.BufferedImage
import java.awt.Color

trait Worker extends Actor {
  override def receive = {
    case Work(lineNo) => {
      val lineResult = calcResult(lineNo)
      sender ! Result(lineNo, lineResult)
    }
  }

  def mapImagePoint(image: BufferedImage, x: Int, y: Int, matrix: Array[Array[Double]]): (Double, Double, Double) = {
    var result = (0.0, 0.0, 0.0)
    
    for {
      dY <- -1 to 1
      dX <- -1 to 1
      cY = y + dY
      cX = x + dX
    } {
      if (cY >= 0 && cY < image.getHeight && cX >= 0 && cX < image.getWidth) {
        val coefficient = matrix(dY + 1)(dX + 1)
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
    
  def calcResult(lineNo: Int): Array[Int]
  
  def sumKernel(k1: Array[Array[Double]], k2: Array[Array[Double]]): Array[Array[Double]] = {
    var s = Array(Array( 0.0, 0.0, 0.0), Array( 0.0, 1.0, 0.0), Array( 0.0, 0.0, 0.0))
    
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        s(x)(y) = k1(x)(y) + k2(x)(y)
      }
    }
    
    s
  }
  
}

class SobelOp(image: BufferedImage) extends Worker {
  val width = image.getWidth
    
  val Gy = Array(Array(-1.0, -2.0, -1.0), Array( 0.0, 0.0, 0.0), Array( 1.0, 2.0, 1.0))
  val Gx = Array(Array(-1.0,  0.0,  1.0), Array(-2.0, 0.0, 2.0), Array(-1.0, 0.0, 1.0))
  
  override def calcResult(lineNo: Int): Array[Int] = {
      var lineResult = new Array[Int](width)
      
      for (x <- 0 until width) {
        val xValue = mapImagePoint(image, x, lineNo, Gx)
        val yValue = mapImagePoint(image, x, lineNo, Gy)
        val xGray = xValue._1 * 0.2126 + xValue._2 * 0.7152 + xValue._3 * 0.0722
        val yGray = yValue._1 * 0.2126 + yValue._2 * 0.7152 + yValue._3 * 0.0722
        val tmp = Math.sqrt(Math.pow(xGray, 2) + Math.pow(yGray, 2))
        val value = Math.min(1.0, tmp).toFloat
        
        lineResult(x) = new Color(value, value, value).getRGB()
      }
      
      lineResult
  }
}

class ThresholdOp(image: BufferedImage, val threshold: Int) extends Worker {
  val width = image.getWidth

  override def calcResult(lineNo: Int): Array[Int] = {
    var lineResult = new Array[Int](width)

    for (x <- 0 until width) {
      val color = new Color(image.getRGB(x, lineNo))

      if (color.getRed() > threshold) {
        lineResult(x) = Color.WHITE.getRGB
      } else {
        lineResult(x) = Color.BLACK.getRGB
      }
    }

    lineResult
  }
}

class SharpenOp(image: BufferedImage, val c: Double) extends Worker {
  val width = image.getWidth

  val noOp = Array(Array( 0.0, 0.0, 0.0), Array( 0.0, 1.0, 0.0), Array( 0.0, 0.0, 0.0))

  val F1   = Array(Array( 0.0, 1.0, 0.0), Array( 1.0,-4.0, 1.0), Array( 0.0, 1.0, 0.0))
  val F2   = Array(Array( 1.0, 1.0, 1.0), Array( 1.0,-8.0, 1.0), Array( 1.0, 1.0, 1.0))

  val Fc  = F1.map(_.map(_ * c))
  
  val Fs = sumKernel(Fc, noOp)

  override def calcResult(lineNo: Int): Array[Int] = {
    var lineResult = new Array[Int](width)

    for (x <- 0 until width) {
      val pixel = mapImagePoint(image, x, lineNo, Fs)
      val r = Math.min(1.0, Math.max(0.0, pixel._1)).toFloat
      val g = Math.min(1.0, Math.max(0.0, pixel._2)).toFloat
      val b = Math.min(1.0, Math.max(0.0, pixel._3)).toFloat
      lineResult(x) = new Color(r, g, b).getRGB
    }
    
    lineResult
  }
}

class BlurOp(image: BufferedImage) extends Worker {
  val width = image.getWidth

  val F1 = Array(Array( 1.0, 1.0, 1.0), Array( 1.0, 1.0, 1.0), Array( 1.0, 1.0, 1.0))

  val Fc = F1.map(_.map(_ * 0.111))

  override def calcResult(lineNo: Int): Array[Int] = {
    var lineResult = new Array[Int](width)

    for (x <- 0 until width) {
      val pixel = mapImagePoint(image, x, lineNo, Fc)
      val r = Math.min(1.0, Math.max(0.0, pixel._1)).toFloat
      val g = Math.min(1.0, Math.max(0.0, pixel._2)).toFloat
      val b = Math.min(1.0, Math.max(0.0, pixel._3)).toFloat
      lineResult(x) = new Color(r, g, b).getRGB
    }
    
    lineResult
  }
}

class NoOp(image: BufferedImage) extends Worker {
  val width = image.getWidth
  
  override def calcResult(lineNo: Int): Array[Int] = {
    var lineResult = new Array[Int](width)
    
    for (x <- 0 until width) {
      lineResult(x) = image.getRGB(x, lineNo)
    }
    
    lineResult
  }
}
