import java.awt.{Color, Graphics}
import javax.swing.{JPanel, JFrame}
import cats.effect.unsafe.implicits._
import cats.effect.IO

class SierpinskiJPanel(x:Int,
                       y: Int,
                       size: Int,
                       minSize: Int,
                       colour: Color
) extends JPanel:

  override def paintComponent(g: Graphics): Unit =
    sierpinskiTriangle(g).unsafeRunSync()

  def sierpinskiTriangle(g: Graphics): IO[Unit] =
    IO{ g.setColor(colour) } >> sierpinskiTriangle(g, x, y, size)

  def sierpinskiTriangle(g: Graphics, x: Int, y: Int, size: Int): IO[Unit] =
    if size <= minSize
    then
      fillTriangle(g, x, y, size)
    else
      val halfSize = size / 2
      sierpinskiTriangle(g, x, y, halfSize) >>
      sierpinskiTriangle(g, x, y - halfSize, halfSize) >>
      sierpinskiTriangle(g, x + halfSize, y, halfSize)

  def fillTriangle(g: Graphics, x: Int, y: Int, size: Int): IO[Unit] =
    val xs = Array(x, x + size, x)
    val ys = Array(y, y, y - size)
    IO{ g.fillPolygon(xs, ys, 3) }

@main def sierpinskiWithIO: Unit =

  val minSize = 8
  val title = "Sierpinski's carpet"
  val windowPosition = (0,0)
  val width = 600
  val height = 600
  val dimensions = (width, height)
  val backgroundColour = Color.white
  val triangleColour = Color.blue
  val triangleSize = 512
  val triangleXPos = 50
  val triangleYPos = 550

  JFrame.setDefaultLookAndFeelDecorated(true)
  val frame = new JFrame("Sierpinski")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setBackground(backgroundColour);
  frame.setSize(width, height);

  val sierpinskiTriangle =
    SierpinskiJPanel(
      triangleXPos,
      triangleYPos,
      triangleSize,
      minSize,
      triangleColour
    )
  frame.add(sierpinskiTriangle);
  frame.setVisible(true)