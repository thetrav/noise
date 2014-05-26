package the.trav

import scala.util.Random

class Perlin(gridSize: Double = 45, val startSeed: Int = Random.nextInt()) {

  def gradient(c:Coord): Coord = {
    //TODO: figure out a better way to do random gradient generation
    val seed = (c.x.toInt+10) + (c.y.toInt*1000)
    //double random because my seed generation sucks
    val rGen = new Random(new Random(seed | startSeed).nextInt())
    def r = (rGen.nextDouble()*2)-1
    Coord(r, r).normalized
  }

  def value(coord: Coord, max: Double): Double = {
    val point = coord / gridSize

    val x = point.x
    val y = point.y

    val x0 = Math.floor(x)
    val x1 = Math.floor(x)+1
    val y0 = Math.floor(y)
    val y1 = Math.floor(y)+1

    val p1 = Coord(x0, y0)
    val p2 = Coord(x1, y0)
    val p3 = Coord(x0, y1)
    val p4 = Coord(x1, y1)

    val g1 = gradient(p1)
    val g2 = gradient(p2)
    val g3 = gradient(p3)
    val g4 = gradient(p4)

    val d1 = point - p1
    val d2 = point - p2
    val d3 = point - p3
    val d4 = point - p4
    
    val i1 = g1 dot d1
    val i2 = g2 dot d2
    val i3 = g3 dot d3
    val i4 = g4 dot d4

    def ease(from: Double, pos: Double) = 3 * Math.pow(pos - from, 2) - 2 * Math.pow(pos - from, 3)
    val sx = ease(x0, x)

    def interpolate(from: Double, to:Double, pos:Double) = (to - from) * pos + from

    val a = interpolate(i1, i2, sx)
    val b = interpolate(i3, i4, sx)

    val sy = ease(y0, y)
    val z = interpolate(a, b, sy)

    Math.abs(z) * max
  }
}
