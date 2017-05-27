package com.example.catfood.ap

import org.scalatest.{ FreeSpec, MustMatchers }
import cats.implicits._

class CartesianSyntaxSpec extends FreeSpec with MustMatchers {

  "raw result is a CartesianBuilder" in {
//    CartesianBuilder[Option]#CartesianBuilder2[Int, Int] - is inaccessible - private to syntax
    val builder = Option(1) |@| Option(2)
    println(builder.getClass)
  }

  // def tupled(implicit invariant: Invariant[F], cartesian: Cartesian[F]): F[(A0, A1)] = Cartesian.tuple2(a0, a1)
  "tupling the results" in {
    (Option(1) |@| Option(2)).tupled mustBe Option((1, 2))
  }

  "mapping over the builder" in {

    // CartesianBuilder2:
    // def map[Z](f: (A0, A1) => Z)(implicit functor: Functor[F], cartesian: Cartesian[F]): F[Z] = Cartesian.map2(a0, a1)(f)
    Option(1) |@| Option(2) map ((i, j) => (i, j)) mustBe Option((1, 2))
  }

  "right projection" in {
    Option(1) *> Option(2) mustBe Option(2)
    Option(1) *> None mustBe None
  }

  "left projection" in {
    Option(1) <* Option(2) mustBe Option(1)
    None <* Option(1) mustBe None
  }

  "contramap" in {}

}
