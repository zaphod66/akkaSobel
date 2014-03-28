package akkaSobel

import java.awt.image.BufferedImage
import javax.swing.JFrame

case class Configuration(image: BufferedImage, noOfWorkers: Int, threshold: Int, filename: String, resImage: BufferedImage, frame: JFrame)
