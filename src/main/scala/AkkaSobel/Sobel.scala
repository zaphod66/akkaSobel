package akkaSobel

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.language.postfixOps

import java.io.File

import java.util.Calendar
import java.text.SimpleDateFormat

import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.{Graphics, BorderLayout, Color, Dimension}
import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.{JComponent, JFrame, JLabel, Timer}

sealed trait OpMessage
case object Start extends OpMessage
case object Stop  extends OpMessage
case class Ops(ops: List[akka.actor.ActorRef])
case class Work(lineNo: Int) extends OpMessage
case class Result(lineNo: Int, lineResult: Array[Int]) extends OpMessage
case class Write(image: BufferedImage, fileName: String, id: Int) extends OpMessage
case class WriteFinished(id: Int) extends OpMessage

object Sobel extends App {
    
  override def main(args: Array[String]): Unit = {
    if (args.length < 2 || args.length > 3) {
      println("Usage: SobelApp <Image> <noOfWorkers> [<threshold>]")
      return
    }
    
    val filename = args(0)
    val noOfWorkers = args(1).toInt
    
    var tmpThreshold = 75
    if (args.length > 2) {
      tmpThreshold = args(2).toInt
    }
    val threshold = tmpThreshold
    
    println("SobelApp " + filename + " -> " + "end_" + filename + " (workers = " + noOfWorkers + " threshold = " + threshold + ")")

    val srcImage = ImageIO.read(new File(filename))
    println("Image: " + srcImage.getWidth() + " * " + srcImage.getHeight())

    val resImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB)

    var f = 1
    val wi = srcImage.getWidth
    val hi = srcImage.getHeight
    var w = wi
    var h = hi
    while (w > 768 || h > 768) {
      f += 1
      w = wi / f
      h = hi / f
    }
    
    println(s"f = $f => ($w, $h)")
    
    val frame = new JFrame("Image Processing with Akka") { frame =>
      setVisible(true)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setPreferredSize(new Dimension(2 * w, h))
      
      object srcComp extends JComponent {
        setPreferredSize(new Dimension(w, h))
        
        override def paintComponent(g: Graphics) {
          g.drawImage(srcImage, 0, 0, w, h, 0, 0, wi, hi, null)
        }
      }
      
      object finComp extends JComponent {
        setPreferredSize(new Dimension(w, h))
        
        override def paintComponent(g: Graphics) {
          g.drawImage(resImage, 0, 0, w, h, 0, 0, wi, hi, null)
        }
      }
      
      object label extends JLabel with ActionListener {
        val time = new Timer(1000, this)
        val df   = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
        
        setBackground(Color.BLACK)
        setForeground(Color.WHITE)
        setOpaque(true)

        setText("Starting " + filename)

        def actionPerformed(event: ActionEvent) {          
          setText("Processing " + filename + " : " + (df format Calendar.getInstance().getTime))
          label.repaint()
        }
        
        def start = time.start
      }
      
      setContentPane(new JComponent {
        setLayout(new BorderLayout)
        add(label, BorderLayout.NORTH)
        add(srcComp, BorderLayout.WEST)
        add(finComp, BorderLayout.EAST)
      })
      pack
      setResizable(false)
      label.start
    }
    
    val config = Configuration(srcImage, noOfWorkers, threshold, filename, resImage, frame)
    calc(config)
  }
  
  def calc(config: Configuration) {
    val system = ActorSystem("SobelSystem")
    
    val master = system.actorOf(Props(new Master(config)), name = "master")

    master ! Start
  }
  
  class Master(val config: Configuration) extends Actor {
    
    val srcImage    = config.image
    val noOfWorkers = config.noOfWorkers
    val threshold   = config.threshold
    val fileName    = config.filename
    
    var noOfLines: Int = _
    val start: Long = System.currentTimeMillis
    val height = srcImage.getHeight
    val width  = srcImage.getWidth
    
    val FINISH_ID = 0

    val embossKernel  = Array(Array(-1.0, -0.5,  0.0),
                              Array(-0.5,  1.0,  0.5),
                              Array( 0.0,  0.5,  1.0))
    
    val embossKernel2 = Array(Array(-2.0, -1.5, -1.0,  0.0,  0.0),
                              Array(-1.5, -1.0, -0.5,  0.0,  0.0),
                              Array(-1.0, -0.5,  1.0,  0.5,  1.0),
                              Array( 0.0,  0.0,  0.5,  1.0,  1.5),
                              Array( 0.0,  0.0,  1.0,  1.5,  2.0))
    
    val embossKernel3 = Array(Array(-3.0, -2.5, -2.0, -1.5,  0.0,  0.0,  0.0),
                              Array(-2.5, -2.0, -1.5, -1.0,  0.0,  0.0,  0.0),
                              Array(-2.0, -1.5, -1.0, -0.5,  0.0,  0.0,  0.0),
                              Array(-1.5, -1.0, -0.5,  1.0,  0.5,  1.0,  1.5),
                              Array( 0.0,  0.0,  0.0,  0.5,  1.0,  1.5,  2.0),
                              Array( 0.0,  0.0,  0.0,  1.0,  1.5,  2.0,  2.5),
                              Array( 0.0,  0.0,  0.0,  1.5,  2.0,  2.5,  3.0))
    
    val sharpenKernel = Array(Array(-1.0, -1.0, -1.0),
                              Array(-1.0,  9.0, -1.0),
                              Array(-1.0, -1.0, -1.0))
    
    val tmp1Image = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB)
    val tmp2Image = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB)

    val writer = context.actorOf(Props(new ImageWriter(start)), name = "imageWriter")

    val sobelRouter    = context.actorOf(Props(new SobelOp(tmp1Image)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sobelRouter")
    val grayRouter     = context.actorOf(Props(new GrayOp(tmp1Image)).withRouter(RoundRobinRouter(noOfWorkers)), name = "grayRouter")
    val thresRouter    = context.actorOf(Props(new ThresholdOp(tmp1Image, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "thresRouter")
    val invertRouter   = context.actorOf(Props(new InvertOp(tmp1Image)).withRouter(RoundRobinRouter(noOfWorkers)), name = "invertRouter")
    val dilate1Router  = context.actorOf(Props(new DilateOp(tmp1Image, 1, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilate1Router")
    val dilate2Router  = context.actorOf(Props(new DilateOp(tmp1Image, 2, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilate2Router")
    val dilate3Router  = context.actorOf(Props(new DilateOp(tmp1Image, 3, threshold)).withRouter(RoundRobinRouter(noOfWorkers)), name = "dilate3Router")
    val sharpenRouter  = context.actorOf(Props(new SharpenOp(tmp1Image, 0.1)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sharpenRouter")
    val blurRouter     = context.actorOf(Props(new BlurOp(tmp1Image)).withRouter(RoundRobinRouter(noOfWorkers)), name = "blurRouter")
    val embossRouter   = context.actorOf(Props(new ConvolveOp(tmp1Image, embossKernel2)).withRouter(RoundRobinRouter(noOfWorkers)), name = "embossRouter")
    val sharpOpRouter  = context.actorOf(Props(new ConvolveOp(tmp1Image, sharpenKernel)).withRouter(RoundRobinRouter(noOfWorkers)), name = "sharpOpRouter")
    val noOpRouter     = context.actorOf(Props(new NoOp(tmp1Image)).withRouter(RoundRobinRouter(noOfWorkers)), name = "noOpRouter")
    
    var ops = List(blurRouter, sobelRouter, grayRouter, thresRouter, dilate1Router, invertRouter, dilate2Router)
//  var ops = List(sobelRouter, noOpRouter)
    
    override def receive = {
      case Start => {
        println("Master Start  after " + (System.currentTimeMillis - start).millis)
        
        val graphics = tmp1Image.getGraphics
        graphics.drawImage(srcImage, 0, 0, null)
        graphics.dispose        
        
        val router = ops.head
        ops = ops.tail
        for (i <- 0 until height) router ! Work(i)
      }
      
      case Stop => {
          val g = config.resImage.getGraphics
          g.drawImage(tmp2Image, 0, 0, null)
          g.dispose

          config.frame.update(config.frame.getGraphics)
          
          println("Master Finish after " + (System.currentTimeMillis - start).millis)
          context.system.shutdown
      }
      
      case Result(lineNo, lineResult) => {
        noOfLines += 1
        
        for (x <- 0 until width) yield {
          val rgb = lineResult(x)
          tmp2Image.setRGB(x, lineNo, rgb)
        }
        
        if (noOfLines == height) {
          if (ops.isEmpty) {
            println("All steps done after " + (System.currentTimeMillis - start).millis)
            writer ! Write(tmp2Image, "end_" + fileName, FINISH_ID)
          } else {
            noOfLines = 0
            println("Step done after " + (System.currentTimeMillis - start).millis)

            val graphics = tmp1Image.getGraphics
            graphics.drawImage(tmp2Image, 0, 0, null)
            graphics.dispose

            val g = config.resImage.getGraphics
            g.drawImage(tmp2Image, 0, 0, null)
            g.dispose

            config.frame.update(config.frame.getGraphics)
            
            val router = ops.head
            ops = ops.tail
            for (i <- 0 until height) router ! Work(i)
          }
        }
      }
      
      case WriteFinished(id) => {
        if (id == FINISH_ID) {
          self ! Stop
        }
      }
    }
  }
  
  class ImageWriter(val startTime: Long) extends Actor {
    override def receive = {
      case Write(image, fileName, id) => {
        println("Write " + id + " (" + fileName + ") started  after " + (System.currentTimeMillis - startTime).millis)
        ImageIO.write(image, "PNG", new File(fileName))
        println("Write " + id + " finished after " + (System.currentTimeMillis - startTime).millis)
        sender ! WriteFinished(id)
      }
    }
  }
  
  // --------------------------------------------------------------------
  
  private def getGrayScale(inputImage: BufferedImage): BufferedImage = {
    val img = new BufferedImage(inputImage.getWidth, inputImage.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val graphics = img.getGraphics
    graphics.drawImage(inputImage, 0, 0, null)
    graphics.dispose()
    img
  }  
}
