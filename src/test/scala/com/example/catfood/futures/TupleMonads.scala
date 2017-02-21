package com.example.catfood.futures

import org.scalatest.{ FreeSpec, MustMatchers }
import cats.implicits._

class TupleMonads extends FreeSpec with MustMatchers {

  "without cats" - {
    "tuples should have no map, flatmap" in {

      """(1, "a") map (_ => "x")""" mustNot typeCheck
      """(1, "a") flatMap { _ => ("b", 3) }""" mustNot typeCheck
    }
  }

  "with cats" - {

    "tuples can be mapped and flatmapped / for-comp" in {

      // the defined map method maps only the second element of the tuple
      // override def map[A, B](fa: (X, A))(f: A => B): (X, B) = (fa._1, f(fa._2))
      val res: (Int, String) = (1, "a") map (_ => "x")
      res mustBe (1, "x")

      val flat = (1, "a") flatMap (_ => (3, "b"))
      flat mustBe (4, "b")

      // strings are concatenated
      val str = ("a", 2) flatMap { _ =>
        ("b", 3)
      }
      str mustBe ("ab", 3)

      // cannot use Some without explicitly typing it as Option:
      // Error value flatMap is not a member of (Some[Int], String)
      // the reason - Semigroup instance exists only for Option, not Some

      val opt = (Option(1), "a") flatMap { s =>
        (Option(2), "b")
      }
      opt mustBe (Option(3), "b")

      val optTuple: (Option[Int], String) = (Some(1), "a")
      val opt2 = optTuple flatMap { _ =>
        (Some(2), "b")
      }
      opt2 mustBe (Option(3), "b")
    }

    // TODO - if tuples can be flatmapped over, why can't case classes?
    case class T(i: Int, s: String)
    """T(1, "a") flatMap (_ => T(2, "b"))""" mustNot typeCheck

  }

}
