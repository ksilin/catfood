package com.example.catfood.futures

import cats.Applicative
import org.scalatest.{ FreeSpec, MustMatchers }
import cats.implicits._

import scala.util.Success

class ImplicitInstanceResolutionSpec extends FreeSpec with MustMatchers {

  def wrapA[A, F[_]](v: A)(implicit F: Applicative[F]): F[A] =
    F.pure(v)

  // F[_] <: Applicative[_]]
  def doubleWrap[A, F[_]](v: F[A])(implicit F: Applicative[F]): F[F[A]] =
    F.pure(v)

  "hinting at an implicit with the value type does not work" in {
    // found scala.util.Try[Int], required Option[Int]
    "val o: Option[Int] = wrapA(1)" mustNot typeCheck
  }

  "the default implicit instance is Try" in {
    // found scala.util.Try[Int], required Option[Int]
    val tr = wrapA(1)
    tr mustBe Success(1)
  }

  "explicit passing of the implicit instance works" in {
    val opt = wrapA(1)(catsStdInstancesForOption)
    opt mustBe Some(1) //an[Option[Int]]
  }

  "explicit parametrization works" in {
    val opt = wrapA[Int, Option](1)
    opt mustBe Some(1) //an[Option[Int]]
  }

  "if the Applicative type is passed in, the correct instance is resolved" in {
    val x = doubleWrap(Option(1))
    x mustBe Some(Some(1))
  }

}
