package the.trav

import org.scalacheck._
import Prop.forAll
import Arbitrary.arbitrary
import Coord._

object CoordSpecification extends Properties("Coord") {

  implicit lazy val arbCoord = Arbitrary( for {
    a <- arbitrary[Int]
    b <- arbitrary[Int]
  } yield Coord(a, b))

  property("subtract zeroes") = forAll {
    (a: Coord) =>
      (a - a) == Origin
  }

  property("add doubles") = forAll {
    (a: Coord) =>
      (a + a) == Coord(a.x * 2, a.y * 2)
  }

  property("scalar multiplication zeroes") = forAll {
    (a: Coord) =>
      (a * 0) == Origin
  }

  property("scalar multiplication unit") = forAll {
    (a: Coord) =>
      (a * 1) == a
  }

  property("scalar division unit") = forAll {
    (a: Coord) =>
      (a / 1) == a
  }

  property("size increases") = forAll {
    (a:Coord) =>
      (a + Unit).size > a.size
  }

  property("size decreases") = forAll {
    (a:Coord) =>
      (a - Unit).size < a.size
  }

  property("normalized") = forAll {
    (a: Coord) =>
      a.normalized.size <= 1
  }

  property("dot product cumulative") = forAll {
    (a: Coord, b:Coord) =>
      a.dot(b) == b.dot(a)
  }

  property("dot product scalar multiplication") = forAll {
    (a:Coord, b:Coord, c: Int) =>
      (a * c).dot(b) == a.dot(b * c)
  }

  property("dot product distributive") = forAll {
    (a: Coord, b:Coord, c: Coord) =>
      a.dot(b + c) == a.dot(b) + a.dot(c)
  }
}
