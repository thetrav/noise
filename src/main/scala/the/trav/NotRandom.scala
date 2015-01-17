package the.trav

import scala.util.Random
import spire.implicits._
import spire.math._

class NotRandom {
  def value(c:Vector[Number], max: Number= 1): Double = {
    val seed: Int = c.foldLeft(0: Number) { (accum:Number, number: Number) => accum * 10 + number * 10 }.toInt
    new Random(new Random(seed).nextInt()).nextInt(max.toInt).toDouble
  }
}
