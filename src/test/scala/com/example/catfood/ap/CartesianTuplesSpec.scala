package com.example.catfood.ap

import org.scalatest.{ FreeSpec, MustMatchers }
import cats.implicits._

class CartesianTuplesSpec extends FreeSpec with MustMatchers {

  val simpleTuple1: (String, Int) = ("a", 1)
  val simpleTuple2: (String, Int) = ("b", 2)

  case class Foo(value: String)

  "tuples containing semigroups are semigroups" in {
    // Semigroup
    val combined = simpleTuple1 |+| simpleTuple2
    combined mustBe ("ab", 3)
  }

  "tuples not containing semigroups in forst place are not semigroups" in {
    val nonSemigroupTuple1 = (Foo("foo"), 4)
    val nonSemigroupTuple2 = (Foo("bar"), 5)
    // Error value |+| is not a member of (CartesianTuplesSpec.this.Foo, Int)
    """nonSemigroupTuple1 |+| nonSemigroupTuple2""" mustNot compile
  }

  "tuples not containing semigroups in last place are not semigroups" in {
    val nonSemigroupTuple3 = (4, Foo("foo"))
    val nonSemigroupTuple4 = (5, Foo("bar"))
    // Error value |+| is not a member of (Int, CartesianTuplesSpec.this.Foo)
    """nonSemigroupTuple3 |+| nonSemigroupTuple4""" mustNot compile
  }

  "tuples have cartesian instances defined" in {

    // Cartesian
    val cart = (simpleTuple1 |@| simpleTuple2).tupled
    println(cart)

    val t3 = ("x", List(1, 2, 3))
    val t4 = ("y", List(4, 5, 6))

    val listed = (t3 |@| t4).tupled

    // the lists are put in a tuple but not concatenated
    listed mustBe ("xy", (List(1, 2, 3), List(4, 5, 6)))
  }

  "tuples containing nonsemigroups in last place cannot be combined with cartesian ops" in {
    val t5 = (Foo("foo"), List(1, 2, 3))
    val t6 = (Foo("bar"), List(4, 5, 6))

    // Error value |@| is not a member of (Foo, List[Int])
    "t5.|@|(t6).tupled" mustNot compile
  }

  "tuples with semigroups in last place can be combined with cartesian ops" in {
    val t5 = (List(1, 2, 3), Foo("foo"))
    val t6 = (List(4, 5, 6), Foo("bar"))

    (t5 |@| t6).tupled mustBe ((List(1, 2, 3, 4, 5, 6), (Foo("foo"), Foo("bar"))))
  }

}
