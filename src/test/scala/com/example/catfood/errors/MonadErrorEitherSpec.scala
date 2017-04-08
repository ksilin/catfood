package com.example.catfood.errors

import cats.implicits._
import com.example.catfood.errors.ApplicationDomain._
import org.scalatest.{ FreeSpec, MustMatchers }

class MonadErrorEitherSpec extends FreeSpec with MustMatchers with ApplicationLogic {

  // after some struggling with the implementation of my own MonadError for Either, the compiler hinted me
  // by failing to compile that there already is one in `cats.instances.catsStdInstancesForEither`
  type Result[A] = Either[NukeException, A]

  "must run" in {
    val res: Result[Impacted] = attack[Result]
    res mustBe Left(MissedByMeters(100))
  }
}
