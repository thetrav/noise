package the.trav

case class Coord(x:Double, y:Double) {
  def -(c:Coord) = Coord(x - c.x, y-c.y)
  def +(c:Coord) = Coord(x + c.x, y+c.y)

  def *(d:Double) = Coord(x*d, y*d)
  def /(d:Double) = Coord(x/d, y/d)

  def normalized = {
    val s = size
    Coord(x/s, y/s)
  }
  def swap = Coord(y, x)

  def size = Math.sqrt(x*x + y*y)
  def dot(c:Coord): Double = x*c.x + y*c.y


}
