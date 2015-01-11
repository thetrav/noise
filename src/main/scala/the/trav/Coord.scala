package the.trav

case class Coord(x:Int, y:Int) {
  def -(c:Coord) = Coord(x - c.x, y-c.y)
  def +(c:Coord) = Coord(x + c.x, y+c.y)

  def *(d:Int) = Coord(x*d, y*d)
  def /(d:Int) = Coord(x/d, y/d)

  def normalized = {
    val s = Math.round(size)
    Coord((x/s).toInt, (y/s).toInt)
  }
  def swap = Coord(y, x)

  def size = Math.sqrt(x*x + y*y)
  def dot(c:Coord): Int = x*c.x + y*c.y
}

object Coord {
  val Unit = Coord(1,1)
  val Origin = Coord(0,0)
}
