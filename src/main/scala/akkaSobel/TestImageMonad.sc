package akkaSobel

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color

import ImageMonad._

object TestImageMonad {
  println("Welcome to ImageMonad")                //> Welcome to ImageMonad
  
  val im = ImageIO.read(new File("/users/nschelle/Downloads/IMG_4525_O.JPG"))
                                                  //> im  : java.awt.image.BufferedImage = BufferedImage@76c39d7f: type = 5 ColorM
                                                  //| odel: #pixelBits = 24 numComponents = 3 color space = java.awt.color.ICC_Col
                                                  //| orSpace@249939c3 transparency = 1 has alpha = false isAlphaPre = false ByteI
                                                  //| nterleavedRaster: width = 2323 height = 3484 #numDataElements 3 dataOff[0] =
                                                  //|  2
  def f1 = { im => ImageMonad(im) }               //> f1: => java.awt.image.BufferedImage => akkaSobel.ImageMonad
  
  val io = for {
    i1 <- sharpen(im, 0.3)
    i1 <- sobel(i1)
    i1 <- thres(i1, 120)
  } yield i1                                      //> flatMap(2323 x 3484)
                                                  //| flatMap(2323 x 3484)
                                                  //| map(2323 x 3484)
                                                  //| io  : akkaSobel.ImageMonad = akkaSobel.ImageMonad@5d4965e5
  val imageout = io.getImage                      //> imageout  : java.awt.image.BufferedImage = BufferedImage@7dc0cd3b: type = 2 
                                                  //| DirectColorModel: rmask=ff0000 gmask=ff00 bmask=ff amask=ff000000 IntegerInt
                                                  //| erleavedRaster: width = 2323 height = 3484 #Bands = 4 xOff = 0 yOff = 0 data
                                                  //| Offset[0] 0
                                                  
  println("writeing...")                          //> writeing...
  ImageIO.write(imageout, "png", new File("/users/nschelle/Downloads/ImageMonad.png"))
                                                  //> res0: Boolean = true
}