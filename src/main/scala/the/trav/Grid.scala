package the.trav

case class Grid(size: Int) {
  assume(size > 0)
  val max = Int.MaxValue / size * size
  val min = -max

  def withinAddressableSpace(scalar:Int): Boolean = {
    scalar >= min && scalar <= max
  }

  def snapLeft(scalar: Int): Int = {
    assume(withinAddressableSpace(scalar))
    val shift = if(scalar < 0) (Math.ceil(scalar.toDouble / -size) * size).toInt else 0
    val s = scalar + shift
    s - s % size - shift
  }
}
