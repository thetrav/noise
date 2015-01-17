package the.trav

import spire.math._

object TestHelpers {
  def approxEq(x: Number, y: Number): Boolean = {
    val error: Number = 0.00000000000001
    def inexact(x: Number) = Interval(x - error, x + error)

    inexact(x) intersects inexact(y)
  }

  def sameResult[A](times: Int, f: => A): Boolean =
    (0 to 10).map(_ => f).forall(_ == f)
}
