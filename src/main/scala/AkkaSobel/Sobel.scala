package akkaSobel

import akka.actor._
import akka.routing.RoundRobinPool
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
case object MasterFinish extends OpMessage
case class Ops(ops: List[akka.actor.ActorRef])
case class Work(lineNo: Int) extends OpMessage
case class Result(lineNo: Int, lineResult: Array[Int]) extends OpMessage
case class Write(image: BufferedImage, fileName: String, id: Int) extends OpMessage
case class WriteFinished(id: Int) extends OpMessage

object Sobel {
    
  def main(args: Array[String]): Unit = {
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
    
    println("Akka App " + filename + " -> " + "end_" + filename + " (workers = " + noOfWorkers + " threshold = " + threshold + ")")

    val srcImage = ImageIO.read(new File(filename))

    val resImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB)

    var f = 1
    val wi = srcImage.getWidth
    val hi = srcImage.getHeight
    var w = wi
    var h = hi
    while (w > 960 || h > 1100) {
      f += 1
      w = wi / f
      h = hi / f
    }
    
    println(s"Size: ($wi, $hi) f = $f => ($w, $h)")
    
    val frame = new JFrame("Image Processing with Akka") { frame =>
      setVisible(true)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setPreferredSize(new Dimension(2 * w, h + 38))
      
      object srcComp extends JComponent {
        setPreferredSize(new Dimension(w, h))
        
        override def paintComponent(g: Graphics) {
          g.drawImage(srcImage, 0, 0, w, h, 0, 0, wi, hi, null)
        }
      }
      
      object resComp extends JComponent {
        setPreferredSize(new Dimension(w, h))
        
        override def paintComponent(g: Graphics) {
          g.drawImage(resImage, 0, 0, w, h, 0, 0, wi, hi, null)
        }
      }
      
      object label extends JLabel with ActionListener {
        val time = new Timer(1000, this)
        val df   = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
        val srt  = Calendar.getInstance().getTime
        
        setBackground(Color.BLACK)
        setForeground(Color.WHITE)
        setOpaque(true)

        setText("Starting " + filename)

        def actionPerformed(event: ActionEvent) {          
          setText(s"Processing $filename ($wi, $hi) - (workers = $noOfWorkers) - " + (df format Calendar.getInstance().getTime) +
                  " - Start: " + (df format srt))
          label.repaint()
        }
        
        def start = time.start
        def stop  = time.stop
      }
      
      setContentPane(new JComponent {
        setLayout(new BorderLayout)
        add(label, BorderLayout.NORTH)
        add(srcComp, BorderLayout.WEST)
        add(resComp, BorderLayout.EAST)
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

    val cannyRouter    = context.actorOf(Props(new CannyOp(tmp1Image, 0.35, 0.15)).withRouter(RoundRobinPool(noOfWorkers)), name = "cannyRouter")

    val sobelMaster    = context.actorOf(Props(new SobelMaster(tmp1Image, tmp2Image, noOfWorkers)), name = "sobelMaster")
    val grayMaster     = context.actorOf(Props(new GrayMaster(tmp1Image, tmp2Image, noOfWorkers)), name = "grayMaster")
    val thresMaster    = context.actorOf(Props(new ThresholdMaster(tmp1Image, tmp2Image, threshold, noOfWorkers)), name = "thresMaster")
    val invertMaster   = context.actorOf(Props(new InvertMaster(tmp1Image, tmp2Image, noOfWorkers)), name = "invertMaster")
    val dilate1Master  = context.actorOf(Props(new DilateMaster(tmp1Image, tmp2Image, 1, threshold, noOfWorkers)), name = "dilate1Master")
    val medianMaster   = context.actorOf(Props(new MedianMaster(tmp1Image, tmp2Image, 2, noOfWorkers)), name = "medianMaster")
    val blurMaster     = context.actorOf(Props(new BlurMaster(tmp1Image, tmp2Image, noOfWorkers)), name = "blurMaster")
    val embossMaster   = context.actorOf(Props(new ConvolveMaster(tmp1Image, tmp2Image, noOfWorkers, embossKernel2, "Emboss")), name = "embossMaster")
    val sharpOpMaster  = context.actorOf(Props(new ConvolveMaster(tmp1Image, tmp2Image, noOfWorkers, sharpenKernel, "Sharpen")), name = "sharpOpMaster")
    
    val cannyMaster    = context.actorOf(Props(new CannyMaster(tmp1Image, tmp2Image, noOfWorkers, 0.45, 0.22)), name = "cannyMaster")
    val noOpMaster     = context.actorOf(Props(new NoOpMaster(tmp1Image, tmp2Image)), name = "NoOpMaster")
    
    var ops = List(blurMaster, cannyMaster, invertMaster)
//  var ops = List(sharpOpMaster, grayMaster, embossMaster)
//  var ops = List(blurRouter, cannyRouter, invertRouter)
    
    override def receive = {
      case Start => {
        println("Master Start  after " + (System.currentTimeMillis - start).millis)
        
        val graphics = tmp1Image.getGraphics
        graphics.drawImage(srcImage, 0, 0, null)
        graphics.dispose        

        val master = ops.head
        ops = ops.tail
        master ! Start
      }
      
      case MasterFinish => {
        if (ops.isEmpty) {
          println("All steps done after " + (System.currentTimeMillis - start).millis)

          val g = config.resImage.getGraphics
          g.drawImage(tmp2Image, 0, 0, null)
          g.dispose
  
          config.frame.update(config.frame.getGraphics)

          writer ! Write(tmp2Image, fileName + "_END.JPG", FINISH_ID)
        } else {
          noOfLines = 0
        //println("Next step after " + (System.currentTimeMillis - start).millis)

          val graphics = tmp1Image.getGraphics
          graphics.drawImage(tmp2Image, 0, 0, null)
          graphics.dispose

          val g = config.resImage.getGraphics
          g.drawImage(tmp2Image, 0, 0, null)
          g.dispose

          config.frame.update(config.frame.getGraphics)
            
          val master = ops.head
          ops = ops.tail
          master ! Start
        }
      }
      
      case Stop => {
        println("Master Stop after " + (System.currentTimeMillis - start).millis)
        context.system.terminate
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
