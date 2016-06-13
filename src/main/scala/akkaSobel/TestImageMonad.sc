package akkaSobel

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color

import ImageMonad._

object TestImageMonad {
  println("Welcome to ImageMonad")                //> Welcome to ImageMonad
  
  val im = ImageIO.read(new File("/users/nschelle/Downloads/IMG_0501.JPG"))
                                                  //> im  : java.awt.image.BufferedImage = BufferedImage@7986f141: type = 5 ColorM
                                                  //| odel: #pixelBits = 24 numComponents = 3 color space = java.awt.color.ICC_Col
                                                  //| orSpace@a4920bc transparency = 1 has alpha = false isAlphaPre = false ByteIn
                                                  //| terleavedRaster: width = 3264 height = 2448 #numDataElements 3 dataOff[0] = 
                                                  //| 2
  def f1 = { im => ImageMonad(im) }               //> f1: => java.awt.image.BufferedImage => akkaSobel.ImageMonad
  
  val io = for {
    i1 <- sharpen(im, 0.2)
    i1 <- f1(i1)
    i1 <- sobel(i1)
    i1 <- thres(i1, 70)
  } yield i1                                      //> flatMap(3264 x 2448)
                                                  //| flatMap(3264 x 2448)
                                                  //| flatMap(3264 x 2448)
                                                  //| map(3264 x 2448)
                                                  //| io  : akkaSobel.ImageMonad = akkaSobel.ImageMonad@7c101300
  val imageout = io.getImage                      //> imageout  : java.awt.image.BufferedImage = BufferedImage@34fa4a2: type = 2 D
                                                  //| irectColorModel: rmask=ff0000 gmask=ff00 bmask=ff amask=ff000000 IntegerInte
                                                  //| rleavedRaster: width = 3264 height = 2448 #Bands = 4 xOff = 0 yOff = 0 dataO
                                                  //| ffset[0] 0
                                                  
  println("writing...")                          //> writing...
  ImageIO.write(imageout, "png", new File("/users/nschelle/Downloads/ImageMonad.png"))
                                                  //> res0: Boolean = true
}