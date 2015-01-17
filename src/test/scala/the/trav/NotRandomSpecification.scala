package the.trav

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.forAll
import spire.implicits._
import spire.math._
import TestHelpers._

object NotRandomSpecification extends Properties("NotRandom") {
  implicit lazy val arbVector = Arbitrary( for {
    a <- Gen.chooseNum(1, Integer.MAX_VALUE)
    b <- Gen.chooseNum(1, Integer.MAX_VALUE)
  } yield Vector[Number](a, b))

  property("generates the same number for the same vector multiple times") = forAll {
    (vector: Vector[Number]) =>
      sameResult(10, { new NotRandom().value(vector) })
  }
}
