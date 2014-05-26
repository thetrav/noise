package the.trav

import scala.util.Random

class NotRandom {
  def value(c:Coord, max: Double = 1): Double = {
    val seed = (c.x.toInt+10) + (c.y.toInt*1000)
    new Random(new Random(seed).nextInt()).nextInt(max.toInt).toDouble
  }
}
