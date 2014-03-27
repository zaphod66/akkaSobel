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

  def mapImagePoint(image: BufferedImage, x: Int, y: Int, kernel: Array[Array[Double]]): (Double, Double, Double) = {
    var result = (0.0, 0.0, 0.0)
    
    val sy = kernel.length
    val sx = kernel(0).length
    
    val by = ( sy - 1 ) / 2
    val bx = ( sx - 1 ) / 2
    
//  println("mapImagePoint: (" + sx + ", " + sy + ") -> (" + bx + ", " + by + ")" )
    
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
      val lineResult = new Array[Int](width)
      
      for (x <- 0 until width) {
        val xValue = mapImagePoint(image, x, lineNo, Gx)
        val yValue = mapImagePoint(image, x, lineNo, Gy)
        val rTmp = Math.sqrt(Math.pow(xValue._1, 2) + Math.pow(yValue._1, 2))
        val gTmp = Math.sqrt(Math.pow(xValue._2, 2) + Math.pow(yValue._2, 2))
        val bTmp = Math.sqrt(Math.pow(xValue._3, 2) + Math.pow(yValue._3, 2))
        val rVal = Math.min(1.0, rTmp).toFloat
        val gVal = Math.min(1.0, gTmp).toFloat
        val bVal = Math.min(1.0, bTmp).toFloat
        
        lineResult(x) = new Color(rVal, gVal, bVal).getRGB()
      }
      
      lineResult
  }
}

class GrayOp(image: BufferedImage) extends Worker {
  val width = image.getWidth

  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)

    for (x <- 0 until width) {
      val color = new Color(image.getRGB(x, lineNo))
      val value = ((color.getRed * 0.2126 + color.getGreen * 0.7152 + color.getBlue * 0.0722) / 255).toFloat
      
      lineResult(x) = new Color(value, value, value).getRGB
    }
    
    lineResult
  }
}

class ThresholdOp(image: BufferedImage, val threshold: Int) extends Worker {
  val width = image.getWidth

  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)

    for (x <- 0 until width) {
//    val rgb   = image.getRGB(x, lineNo)
      val color = new Color(image.getRGB(x, lineNo))

//    println(s"thres => rgb($x, $lineNo) = $rgb, Color(rgb) = $color")
      
      if (color.getRed() > threshold) {
        lineResult(x) = Color.WHITE.getRGB
      } else {
        lineResult(x) = Color.BLACK.getRGB
      }
    }

    lineResult
  }
}

class InvertOp(image: BufferedImage) extends Worker {
  val width = image.getWidth

  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)

    for (x <- 0 until width) {
      val color = new Color(image.getRGB(x, lineNo))
      
      val r = 255 - color.getRed()
      val g = 255 - color.getGreen()
      val b = 255 - color.getBlue()
      
      lineResult(x) = new Color(r, g, b).getRGB()
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
    val lineResult = new Array[Int](width)

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

  val F1 = Array(Array( 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0))
  val F2 = Array(Array( 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0))
  val F3 = Array(Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                 Array( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0))
  val F4 = Array(Array(0.00000067, 0.00002292, 0.00019117, 0.00038771, 0.00019117, 0.00002292, 0.00000067),
                 Array(0.00002292, 0.00078634, 0.00655965, 0.01330373, 0.00655965, 0.00078633, 0.00002292),
                 Array(0.00019117, 0.00655965, 0.05472157, 0.11098164, 0.05472157, 0.00655965, 0.00019117),
                 Array(0.00038771, 0.01330373, 0.11098164, 0.22508352, 0.11098164, 0.01330373, 0.00038771),
                 Array(0.00019117, 0.00655965, 0.05472157, 0.11098164, 0.05472157, 0.00655965, 0.00019117),
                 Array(0.00002292, 0.00078634, 0.00655965, 0.01330373, 0.00655965, 0.00078633, 0.00002292),
                 Array(0.00000067, 0.00002292, 0.00019117, 0.00038771, 0.00019117, 0.00002292, 0.00000067))
  
//val Fc = F1.map(_.map(_ * (1.0 /  9.0)))
//val Fc = F2.map(_.map(_ * (1.0 / 25.0)))
//val Fc = F3.map(_.map(_ * (1.0 / 49.0)))
  val Fc = F4

  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)

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

class MedianOp(image: BufferedImage, radius: Int) extends Worker {
  val width = image.getWidth
  
  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)
    
    lineResult
  }
}

class DilateOp(image: BufferedImage, radius: Int, thres: Int) extends Worker {
  val width = image.getWidth
  
  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)

    for (x <- 0 until width) {
      var maxRGB = 0
      
      for {
        dY <- -radius to radius
        dX <- -radius to radius
        cY = lineNo + dY
        cX = x + dX
      } {
        if (cY >= 0 && cY < image.getHeight && cX >= 0 && cX < width) {
          val rgb = image.getRGB(cX, cY)
          if (Math.abs(rgb) > Math.abs(maxRGB)) {
            maxRGB = rgb
          }
        
      //  println(s"lineNo = $lineNo ===> x = $x, dX = $dX, dY = $dY, cX = $cX, cY = $cY => rgb = $rgb, maxRGB = $maxRGB")
        
        }
      }

      if (new Color(maxRGB).getRed > thres) {
        lineResult(x) = Color.WHITE.getRGB
      } else {
        lineResult(x) = Color.BLACK.getRGB
      }
    }
    
    lineResult
  }
}

class ConvolveOp(image: BufferedImage, kernel: Array[Array[Double]]) extends Worker {
  val width = image.getWidth
  
  override def calcResult(lineNo: Int): Array[Int] = {
    val lineResult = new Array[Int](width)
    
    for (x <- 0 until width) {
      val pixel = mapImagePoint(image, x, lineNo, kernel)
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
    val lineResult = new Array[Int](width)
    
    for (x <- 0 until width) {
      lineResult(x) = image.getRGB(x, lineNo)
    }
    
    lineResult
  }
}
