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

  def value(coord: Vector[Number], max: Number = 1): Number = {
    assume(coord.length == 2)
    val x = coord.head
    val y = coord.tail.head

    val x0 = grid.snapLeft(x)
    val y0 = grid.snapLeft(y)
    val x1 = x0 + grid.size
    val y1 = y0 + grid.size

    val p1 = Vector(x0, y0)
    val p2 = Vector(x1, y0)
    val p3 = Vector(x0, y1)
    val p4 = Vector(x1, y1)

    val g1 = gradient(p1)
    val g2 = gradient(p2)
    val g3 = gradient(p3)
    val g4 = gradient(p4)

    val d1 = (coord - p1) :/ grid.size
    val d2 = (coord - p2) :/ grid.size
    val d3 = (coord - p3) :/ grid.size
    val d4 = (coord - p4) :/ grid.size

    val i1 = g1 ⋅ d1
    val i2 = g2 ⋅ d2
    val i3 = g3 ⋅ d3
    val i4 = g4 ⋅ d4
    val sx = ease(x0/x1, x/x1)

    val a = interpolate(i1, i2, sx)
    val b = interpolate(i3, i4, sx)

    val sy = ease(y0/y1, y/y1)
    val z = interpolate(a, b, sy)

    abs(z) * max
  }
}
