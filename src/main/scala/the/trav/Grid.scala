package the.trav
import spire.implicits._
import spire.math._

case class Grid(size: Number) {
  assume(size > 0)

  def snapLeft(scalar: Number): Number = {
    val shift: Number = if(scalar < 0) floor(ceil(scalar / -size) * size) else 0
    val s = scalar + shift
    s - s % size - shift
  }
}
