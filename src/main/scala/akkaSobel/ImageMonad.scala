package akkaSobel

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color

import ImageMonad._

class ImageMonad(image: BufferedImage) {
  def map(f: BufferedImage => BufferedImage): ImageMonad = {
    val width  = image.getWidth
    val height = image.getHeight
      
    println(s"map($width x $height)")
    ImageMonad(f(image))
  }
    
  def flatMap(f: BufferedImage => ImageMonad): ImageMonad = {
    val width  = image.getWidth
    val height = image.getHeight
      
    println(s"flatMap($width x $height)")
    f(image)
  }
  
//  def write(fileName: String): Unit = {
//    ImageIO.write(image, "png", new File(fileName))
//  }
  
  def getImage: BufferedImage = image
}

object ImageMonad {
  def apply(image: BufferedImage) = { new ImageMonad(image) }
  
  type KernelFunction = BufferedImage => ImageMonad
  
  // no op
  def noOp: KernelFunction = { im => ImageMonad(im) }
  
  // gray level image
  def gray: KernelFunction = { im =>
    val width = im.getWidth
    val height = im.getHeight
    
    val tmpImage = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    
    for (y <- 0 until height)
      for (x <- 0 until width) {
        val color = new Color(im.getRGB(x, y))
        val gray  = color.getRed * 0.2126 + color.getGreen * 0.7152 + color.getBlue * 0.0722
        
        val gCol  = new Color(gray.toInt, gray.toInt, gray.toInt)
        tmpImage.setRGB(x, y, gCol.getRGB())
      }
    
    ImageMonad(tmpImage)
  }
  
  // threshold
  def thres(im: BufferedImage, threshold: Int): ImageMonad = {
    val width = im.getWidth
    val height = im.getHeight
    
    val tmpImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val color = new Color(im.getRGB(x, y))
          val thres = ( color.getRed + color.getGreen + color.getBlue ) / 3
          val value = if (thres > threshold) 255 else 0
          val newcol = new Color(value, value, value)
          
          tmpImage.setRGB(x, y, newcol.getRGB)
      }
    }
    
    ImageMonad(tmpImage)
  }
  
  // sobel operator
  def sobel: KernelFunction = { im =>
    val width = im.getWidth
    val height = im.getHeight
    
    val tmpImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    
    val Gy = Array(Array(-1.0, -2.0, -1.0), Array( 0.0, 0.0, 0.0), Array( 1.0, 2.0, 1.0))
    val Gx = Array(Array(-1.0,  0.0,  1.0), Array(-2.0, 0.0, 2.0), Array(-1.0, 0.0, 1.0))
    
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val xValue = convolvePoint(im, x, y, Gx)
        val yValue = convolvePoint(im, x, y, Gy)
        val xGray = xValue._1 * 0.2126 + xValue._2 * 0.7152 + xValue._3 * 0.0722
        val yGray = yValue._1 * 0.2126 + yValue._2 * 0.7152 + yValue._3 * 0.0722
        val tmp = Math.sqrt(Math.pow(xGray, 2) + Math.pow(yGray, 2))
        val value = Math.min(1.0, tmp).toFloat
        
        val color = new Color(value, value, value)
        tmpImage.setRGB(x, y, color.getRGB())
      }
    }

    ImageMonad(tmpImage)
  }
  
  def sharpen(im: BufferedImage, c: Double): ImageMonad = {  
    val width = im.getWidth
    val height = im.getHeight
    
    val tmpImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    val noOp = Array(Array( 0.0, 0.0, 0.0), Array( 0.0, 1.0, 0.0), Array( 0.0, 0.0, 0.0))

    val F1 = Array(Array( 0.0,-1.0, 0.0), Array(-1.0, 4.0,-1.0), Array( 0.0,-1.0, 0.0))
    val F2 = Array(Array(-1.0,-1.0,-1.0), Array(-1.0, 8.0,-1.0), Array(-1.0,-1.0,-1.0))

    val F1c = F1.map(_.map(_ * c))
    val F2c = F2.map(_.map(_ * c))
    
    val F1s = sumKernel(F1c, noOp)
    
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val convo = convolvePoint(im, x, y, F1s)
        val pixel = im.getRGB(x, y)
        val r = Math.min(1.0, Math.max(0.0, convo._1)).toFloat
        val g = Math.min(1.0, Math.max(0.0, convo._2)).toFloat
        val b = Math.min(1.0, Math.max(0.0, convo._3)).toFloat
        
        val color = new Color(r, g, b)
        tmpImage.setRGB(x, y, color.getRGB())
      }
    }
    
    ImageMonad(tmpImage)
  }
  
  private def convolvePoint(image: BufferedImage, x: Int, y: Int, matrix: Array[Array[Double]]): (Double, Double, Double) = {
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
  
  private def sumKernel(k1: Array[Array[Double]], k2: Array[Array[Double]]): Array[Array[Double]] = {
    var s = Array(Array( 0.0, 0.0, 0.0), Array( 0.0, 1.0, 0.0), Array( 0.0, 0.0, 0.0))
    
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        s(x)(y) = k1(x)(y) + k2(x)(y)
      }
    }
    
//    printk("k1: ", k1)
//    printk("k2: ", k2)
//    printk("s:  ",  s)
    
    s
  }
  
  private def printk(line: String, k: Array[Array[Double]]): Unit = {
    print(line)
    
    print(" (")
    for (y <- 0 until 3) {
      print(" (")
      for (x <- 0 until 2) {
        print(k(x)(y) + ", ")
      }
      print(k(2)(y) + ") ")
    }
    println(")")
  }
}