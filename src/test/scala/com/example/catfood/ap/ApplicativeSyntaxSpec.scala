package com.example.catfood.ap

import cats.implicits._
import org.scalatest.{ FreeSpec, MustMatchers }

class ApplicativeSyntaxSpec extends FreeSpec with MustMatchers {

  "applicative addition" in {
//      Some(1) |+| Some(2) mustBe Some(3) - |+| is not a member of Some
    Option(1) |+| Option(2) mustBe Option(3)
  }

}
