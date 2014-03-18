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
    
  def mapImagePoint(image: BufferedImage, x: Int, y: Int, matrix: Array[Array[Int]]): (Double, Double, Double) = {
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
    
  def calcResult(lineNo: Int): Array[(Float,Float,Float)]
  
  def getName: String
}

class SobelOp(image: BufferedImage) extends Worker {
  val width = image.getWidth
    
  val Gy = Array(Array(-1, -2, -1), Array(0, 0, 0), Array(1, 2, 1))
  val Gx = Array(Array(-1, 0, 1), Array(-2, 0, 2), Array(-1, 0, 1))
  
  override def calcResult(lineNo: Int): Array[(Float, Float, Float)] = {
      var lineResult = new Array[(Float, Float, Float)](width)
      
      for (x <- 0 until width) {
        val xValue = mapImagePoint(image, x, lineNo, Gx)
        val yValue = mapImagePoint(image, x, lineNo, Gy)
        val xGray = xValue._1 * 0.2126 + xValue._2 * 0.7152 + xValue._3 * 0.0722
        val yGray = yValue._1 * 0.2126 + yValue._2 * 0.7152 + yValue._3 * 0.0722
        val tmp = Math.sqrt(Math.pow(xGray, 2) + Math.pow(yGray, 2))
        val value = Math.min(1.0, tmp).toFloat
        
        lineResult(x) = (value, value, value)
      }
      
      lineResult
  }
  
  override def getName = "Sobel"
}

class ThresholdOp(image: BufferedImage, val threshold: Int) extends Worker {
  val width = image.getWidth

  override def calcResult(lineNo: Int): Array[(Float, Float, Float)] = {
    var lineResult = new Array[(Float, Float, Float)](width)

    for (x <- 0 until width) {
      val color = new Color(image.getRGB(x, lineNo))

      if (color.getRed() > threshold) {
        lineResult(x) = (1.0f, 1.0f, 1.0f)
      } else {
        lineResult(x) = (0.0f, 0.0f, 0.0f)
      }
    }

    lineResult
  }
  
  override def getName = "Threshold"
}

class SharpenOp(image: BufferedImage, val c: Double) extends Worker {
  val width = image.getWidth

//val F1 = Array(Array( 0, 1, 0), Array( 1,-4, 1), Array( 0, 1, 0))
//val F2 = Array(Array( 1, 1, 1), Array( 1,-8, 1), Array( 1, 1, 1))
  
  val F1 = Array(Array( 0, 1, 0), Array( 1,-5, 1), Array( 0, 1, 0))
  val F2 = Array(Array( 1, 1, 1), Array( 1,-9, 1), Array( 1, 1, 1))
  
  override def calcResult(lineNo: Int): Array[(Float, Float, Float)] = {
    var lineResult = new Array[(Float, Float, Float)](width)

    for (x <- 0 until width) {
      val pixel = mapImagePoint(image, x, lineNo, F1)
      val r = Math.min(1.0, Math.abs(pixel._1)).toFloat
      val g = Math.min(1.0, Math.abs(pixel._2)).toFloat
      val b = Math.min(1.0, Math.abs(pixel._3)).toFloat
      lineResult(x) = (r, g, b)
    }
    
    lineResult
  }
  
  override def getName = "Sharpen"
}

class NoOp(image: BufferedImage) extends Worker {
  val width = image.getWidth
  
  override def calcResult(lineNo: Int): Array[(Float, Float, Float)] = {
    var lineResult = new Array[(Float, Float, Float)](width)
    
    for (x <- 0 until width) {
      val color = new Color(image.getRGB(x, lineNo))
      lineResult(x) = ((color.getRed()   / 255.0).toFloat,
                       (color.getGreen() / 255.0).toFloat,
                       (color.getBlue()  / 255.0).toFloat)
    }
    
    lineResult
  }
  
  override def getName = "NoOp"
}