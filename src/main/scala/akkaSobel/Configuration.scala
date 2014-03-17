package akkaSobel

import java.awt.image.BufferedImage

case class Configuration(image: BufferedImage, noOfWorkers: Int, threshold: Int, filename: String)
