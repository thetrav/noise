package the.trav

import org.scalacheck._
import Prop.{forAll, BooleanOperators}
import Arbitrary._
import scala.util.Try

object GridSpecification extends Properties("Grid") {
  implicit lazy val arbGrid = Arbitrary( for {
    a <- Gen.chooseNum(1, Integer.MAX_VALUE)
  } yield new Grid(a))

  property("all cells within addressable space snap left") = forAll {
    (scalar: Int, grid: Grid) =>
      grid.withinAddressableSpace(scalar) ==> {
        val snap = grid.snapLeft(scalar)
        (snap <= scalar)        :| "snap shifted right" &&
        (snap % grid.size == 0) :| "snap not on grid"
      }
  }

  property("all cells outside addressable space fail at runtime") = forAll {
    (scalar: Int, grid: Grid) =>
      !grid.withinAddressableSpace(scalar) ==> {
        Try(grid.snapLeft(scalar)).isFailure
      }
  }
}
