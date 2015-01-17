package the.trav

import javax.swing._
import java.awt.{Graphics, BorderLayout, Color}
import spire.implicits._
import java.awt.image.BufferedImage


object Main extends App {

  val width = 800
  val height = 600

  val frame = new JFrame("travs app")
  frame.setSize(width, height)

  val panel = new JPanel()
  panel.setLayout(new BorderLayout())

  val controls = new JPanel()
  panel.add(controls, BorderLayout.SOUTH)

  val canvas = new JPanel()
  canvas.setSize(width-100, height-200)
//  panel.add(canvas, BorderLayout.CENTER)

  def noisePanel(w:Int, h:Int, perlin: Perlin): JPanel = {
    val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    val graphics = image.getGraphics
    (0 to w).foreach { x =>
      (0 to h).foreach { y =>
        val v = (perlin.value(Vector(x,y)) * 200).toInt
        graphics.setColor(new Color(v, v, v))
        graphics.fillRect(x, y, 1, 1)
//        def dot(n:Int, c:Color) {
//          if(x % n == 0 && y % n == 0) {
//            graphics.setColor(c)
//            graphics.fillRect(x,y,1,1)
//          }
//        }
//        dot(10, Color.red)
//        dot(50, Color.yellow)
    }}
    new JPanel() {
      override def paintComponent(g:Graphics) {
        g.setColor(new Color(255,255,255))
        g.fillRect(0,0,this.getWidth, this.getHeight)
        g.drawImage(image, 0, 0, this)
      }
    }
  }



  frame.getContentPane.add(noisePanel(width,height, Perlin(Grid(50))))

  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)
}