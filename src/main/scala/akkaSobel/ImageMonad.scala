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
  
  def write(fileName: String): Unit = {
    ImageIO.write(image, "png", new File(fileName))
  }
}

object ImageMonad {
  def apply(image: BufferedImage) = { new ImageMonad(image) }
  
  type KernelFunction = BufferedImage => ImageMonad
  
  def noOp: KernelFunction = { im => ImageMonad(im) }
  def gray: KernelFunction = { im =>
    val width = im.getWidth
    val height = im.getHeight
    
//  val tmpImage = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val tmpImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    
    for (x <- 0 until width)
      for (y <- 0 until height) {
        val color = new Color(im.getRGB(x, y))
        val gray  = color.getRed * 0.2126 + color.getGreen * 0.7152 + color.getBlue * 0.0722
        
        val gCol  = new Color(gray.toInt, gray.toInt, gray.toInt)
        tmpImage.setRGB(x, y, gCol.getRGB())
      }
    
    ImageMonad(tmpImage)
  }
}