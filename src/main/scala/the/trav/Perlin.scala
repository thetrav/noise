package the.trav

import scala.util.Random

class Perlin(grid: Grid, val startSeed: Int = Random.nextInt()) {
  def gradient(c:Coord): Coord = {
    //TODO: figure out a better way to do pseudo random gradient generation
    val seed = (c.x.toInt+10) + (c.y.toInt*1000)
    //double random because my seed generation produces a homogeneous pattern
    val rGen = new Random(new Random(seed | startSeed).nextInt())
    def r = (rGen.nextInt()*2)-1
    Coord(r, r).normalized
  }

  def value(coord: Coord, max: Double): Double = {
    val x0 = grid.snapLeft(coord.x)
    val y0 = grid.snapLeft(coord.y)
    val x1 = x0 + grid.size
    val y1 = y0 + grid.size

    val p1 = Coord(x0, y0)
    val p2 = Coord(x1, y0)
    val p3 = Coord(x0, y1)
    val p4 = Coord(x1, y1)

    val g1 = gradient(p1)
    val g2 = gradient(p2)
    val g3 = gradient(p3)
    val g4 = gradient(p4)

    val d1 = coord - p1
    val d2 = coord - p2
    val d3 = coord - p3
    val d4 = coord - p4
    
    val i1 = g1 dot d1
    val i2 = g2 dot d2
    val i3 = g3 dot d3
    val i4 = g4 dot d4

    def ease(from: Double, pos: Double) = 3 * Math.pow(pos - from, 2) - 2 * Math.pow(pos - from, 3)
    val sx = ease(x0, coord.x)

    def interpolate(from: Double, to:Double, pos:Double) = (to - from) * pos + from

    val a = interpolate(i1, i2, sx)
    val b = interpolate(i3, i4, sx)

    val sy = ease(y0, coord.y)
    val z = interpolate(a, b, sy)

    Math.abs(z) * max
  }
}
