package the.trav

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import spire.implicits._
import spire.math._
import TestHelpers._

object PerlinSpecification extends Properties("Perlin") {
  val half: Number = 0.5
  val zero: Number = 0
  val one: Number = 1

  implicit lazy val arbGrid = GridSpecification.arbGrid
  implicit lazy val arbVector = NotRandomSpecification.arbVector
  implicit lazy val arbPerlin = Arbitrary( for {
    grid <- Arbitrary.arbitrary[Grid]
    seed <- Gen.chooseNum(1, Integer.MAX_VALUE)
  } yield Perlin(grid, seed))

  def size(v: Vector[Number]) = sqrt(v.foldLeft(zero){ (b, n) => b + n * n})

  property("gradient generates normalised Vectors") = forAll {
    (perlin: Perlin, vector: Vector[Number]) =>
      approxEq(one, size(perlin.gradient(vector)))
  }

  property("gradient generates a constant number for a given vector") = forAll {
    (perlin: Perlin, vector: Vector[Number]) =>
      sameResult(10, { perlin.gradient(vector) })
  }

  property("easing function generates a normalized value for normalized input") = forAll {
    (from: Int) =>
      val eased = Perlin.ease(from, from + random)
      eased >= zero && eased <= one
  }

  property("easing function emphasizes edges") = forAll {
    (from: Int) =>
      val r: Number = random()
      val to = from + r
      val eased: Number = Perlin.ease(from, to)

      (r == zero && eased == zero) ||
      (r == one  && eased == one)  ||
      (r == half && eased == half) ||
      (r <  half && eased < r)     ||
      (r >  half && eased > r)
  }

  property("linear interpolation in straight forward case") = forAll {
    (a: Double) =>
      val amount = a/Double.MaxValue
      Perlin.interpolate(0, 1, amount) == (amount: Number)
  }

  property("linear interpolation on negative scaled axis") = forAll {
    (a: Double) =>
      val amount = a/Double.MaxValue
      Perlin.interpolate(0, -100, amount) == (amount: Number) * -100
  }

  property("noise value gives a normalized figure") = forAll {
    (perlin:Perlin, vector: Vector[Number]) =>
      val noise: Number = perlin.value(vector)
      noise >= zero &&
      noise <= one
  }

  property("noise value is consistent") = forAll {
    (perlin: Perlin, vector: Vector[Number]) =>
       sameResult(10, { perlin.value(vector)} )
  }
}