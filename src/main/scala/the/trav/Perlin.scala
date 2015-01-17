package the.trav

import scala.util.Random
import spire.implicits._
import spire.math._

object Perlin {
  def ease(from: Number, to: Number) = {
    val dif = to - from
    assume(dif >= 0 && dif <= 1)
    3 * (dif pow 2) - 2 * (dif pow 3)
  }

  def interpolate(from: Number, to: Number, amount: Number) =
    (to - from) * amount + from
}

case class Perlin(grid: Grid, startSeed: Int = Random.nextInt()) {
  import Perlin._

  def gradient(c:Vector[Number]): Vector[Number] = {
    //TODO: figure out a better way to do pseudo random gradient generation
    val seed: Int = c.foldLeft(0: Number) { (accum:Number, number: Number) => accum * 10 + number * 10 }.toInt
    //double random because my seed generation produces a homogeneous pattern
    val rGen = new Random(new Random(seed | startSeed).nextInt())
    def r = (rGen.nextInt()*2)-1
    Vector[Number](r,r).normalize
  }

  def value(coord: Vector[Number]): Number = {
    assume(coord.length == 2)
    val x = coord.head
    val y = coord.tail.head

    val x0 = grid.snapLeft(x)
    val y0 = grid.snapLeft(y)
    val x1 = x0 + grid.size
    val y1 = y0 + grid.size

    def difference(p:Vector[Number]) = (coord - p) :/ grid.size
    def influence(p:Vector[Number]) = gradient(p) ⋅ difference(p)

    val i1 = influence(Vector(x0, y0))
    val i2 = influence(Vector(x1, y0))
    val i3 = influence(Vector(x0, y1))
    val i4 = influence(Vector(x1, y1))

    val sx = ease(x0 / grid.size, x / grid.size)

    val a = interpolate(i1, i2, sx)
    val b = interpolate(i3, i4, sx)

    val sy = ease(y0 / grid.size, y / grid.size)
    val z = interpolate(a, b, sy)

    abs(z)
  }
}
