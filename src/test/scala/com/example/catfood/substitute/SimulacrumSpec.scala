package com.example.catfood.substitute

import org.scalatest.{ FreeSpec, MustMatchers }

class SimulacrumSpec extends FreeSpec with MustMatchers {

  "using a typeclass with a custom op" in {

    implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
      def append(x: Int, y: Int) = x + y
    }

    import Semigroup.ops._

    val res = 2 |+| 3
    res mustBe 5
  }

  // https://github.com/mpilquist/simulacrum/blob/master/examples/src/test/scala/simulacrum/examples/examples.scala
}
