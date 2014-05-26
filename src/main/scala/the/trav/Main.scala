package the.trav

import scala.swing._
import java.awt.Color
import scala.util.Random
import scala.swing.event.ButtonClicked


object Main extends SimpleSwingApplication {

  val width = 800
  val height = 600

  type PaintFn = Graphics2D => Unit
  type Gen = Coord => Double

//  val panels: Seq[PaintFn] = Seq(random, notRandom, perlin)
//  val panels: Seq[PaintFn] = Seq(notRandom)
  val panels: Seq[PaintFn] = Seq(perlin)

  def perlin = {
    (graphics: Graphics2D) => {
      val p1 = new Perlin(207)
      val p2 = new Perlin(200)
      val p3 = new Perlin(50)

      def r(c:Coord, max: Double) = {
        def p(perlin: Perlin) = perlin.value(c, max)
//        p(p1)
//        val rnd = p(p1) + p(p2)

//        val rnd = (p(p1) + p(p2)) - p(p3)
        val rnd = p(p1)/2 + p(p2)/2 * p(p3)
//        val rnd = p(p1)/2 + p(p2)/1.5 - p(p3)/3
//        val rnd = Math.pow(p(p3)/10, p(p1)/10)
        val z = Math.max(Math.min(rnd, max), 0)
        z
//        z match {
//          case 300 => max
//          case 250 => 0.0
//          case x => x
//        }
      }
      def b = r _
      def g = r _


      paintNoise(graphics, c => r(c, 255), c => g(c, 255), c => b(c, 255))
      graphics.setColor(Color.green)
      graphics.drawString(s"seeds: ${p1.startSeed}, ${p2.startSeed}, ${p3.startSeed}", 350, 350)
    }
  }

  val rGen = new Random()
  def random = (g: Graphics2D) => {
    var n = 0.0
    val r = (c:Coord) => {
      n = rGen.nextInt(255).toDouble
      n
    }
    paintNoise(g, r, _ => n ,_ => n)
  }

  val nGen = new NotRandom()
  def notRandom = (g:Graphics2D) => {
    val r = (c:Coord) => nGen.value(c, 255)
    paintNoise(g, r,r,r)
  }

  def paintNoise(graphics: Graphics2D, r: Gen, g: Gen, b: Gen ) = {
    (0 to width/panels.size).foreach { x =>
      (0 to 50+height/2).foreach { y =>
        val c = Coord(x,y)

        graphics.setColor(new Color(r(c).toInt, g(c).toInt, b(c).toInt))
        graphics.drawRect(x,y,1,1)
      }
    }
  }

  def drawPanel(paintFunction: Graphics2D => Unit) = {
    lazy val canvas = new Panel {
      override def paintComponent(g: Graphics2D) = {
        super.paintComponent(g)
        paintFunction(g)
      }
    }

    lazy val redraw = new Button {
      text = "redraw"
      reactions += {
        case ButtonClicked(_) => canvas.repaint()
      }
    }

    new GridPanel(2, 1) {
      contents.append(canvas, redraw)
    }
  }

  override def top: Frame = new MainFrame {
    title = "Hello, World!"
    contents = new GridPanel(1,panels.size) {
      contents.append(panels.map(drawPanel) :_*)
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }
    size = new Dimension(1000, 800)
  }
}