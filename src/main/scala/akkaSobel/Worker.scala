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
    
  def mapImagePoint(image: BufferedImage, x: Int, y: Int, matrix: Array[Array[Int]]): Double = {
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
    
  def calcResult(lineNo: Int): Array[Double]
}

class SobelOp(image: BufferedImage) extends Worker {
  val width = image.getWidth
    
  val Gy = Array(Array(-1, -2, -1), Array(0, 0, 0), Array(1, 2, 1))
  val Gx = Array(Array(-1, 0, 1), Array(-2, 0, 2), Array(-1, 0, 1))
  
  override def calcResult(lineNo: Int): Array[Double] = {
      var lineResult = new Array[Double](width)
      
      for (x <- 0 until width) {
        val xValue = mapImagePoint(image, x, lineNo, Gx)
        val yValue = mapImagePoint(image, x, lineNo, Gy)
        val tmp = Math.sqrt(Math.pow(xValue, 2) + Math.pow(yValue, 2))
        val value = Math.min(1.0, tmp).toFloat
        
        lineResult(x) = value
      }
      
      lineResult
  }
}

class ThresholdOp(image: BufferedImage, val threshold: Int) extends Worker {
  val width = image.getWidth
  
  override def calcResult(lineNo: Int): Array[Double] = {
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
