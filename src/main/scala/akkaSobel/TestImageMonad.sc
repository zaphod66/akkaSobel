package akkaSobel

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import ImageMonad._

object TestImageMonad {
  println("Welcome to ImageMonad")                //> Welcome to ImageMonad
  
  val image = ImageIO.read(new File("/users/nschelle/Downloads/IMG_4525_O.JPG"))
                                                  //> image  : java.awt.image.BufferedImage = BufferedImage@20ca8d94: type = 5 Col
                                                  //| orModel: #pixelBits = 24 numComponents = 3 color space = java.awt.color.ICC_
                                                  //| ColorSpace@5215005d transparency = 1 has alpha = false isAlphaPre = false By
                                                  //| teInterleavedRaster: width = 2323 height = 3484 #numDataElements 3 dataOff[0
                                                  //| ] = 2
  def f1 = { im => ImageMonad(im) }               //> f1: => java.awt.image.BufferedImage => akkaSobel.ImageMonad
  def f2 = { im => ImageMonad(im) }               //> f2: => java.awt.image.BufferedImage => akkaSobel.ImageMonad

  val imageout = for {
    i1 <- f1(image)
    i1 <- f2(i1)
    i1 <- noOp(i1)
    i1 <- gray(i1)
  } yield i1                                      //> flatMap(2323 x 3484)
                                                  //| flatMap(2323 x 3484)
                                                  //| flatMap(2323 x 3484)
                                                  //| map(2323 x 3484)
                                                  //| imageout  : akkaSobel.ImageMonad = akkaSobel.ImageMonad@787bd4fa
  
  println("writeing...")                          //> writeing.../
  imageout.write("/users/nschelle/Downloads/ImageMonad.png")
}