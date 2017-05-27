package com.example.catfood.futures

import org.scalatest.{ FreeSpec, MustMatchers }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import cats.implicits._

class FutureOptSpec extends FreeSpec with MustMatchers {

  val f1: Int => Future[Option[Int]] =
    i => Future.successful(Some(i + 1))

  val f2: Int => Future[Option[Int]] =
    i => Future.successful(Some(i * 10))

  val g1: Int => Future[List[Int]] =
    i => Future.successful(List(i, i + 1))

  "naive composition" in {
    // expects a Future[Option], has Option
    """ val res = for {
      mI: Option[Int] <- f1(0)
      m: Int <- mI
      mI2 <- f2(m)
    } yield mI2 """ mustNot typeCheck
  }

  "naive comp same type" in {

    val fo: Future[Option[Int]] = f1(0)

    def repack[A](v: Option[A], f: A => Future[Option[A]]): Future[Option[A]] =
      v match {
        case Some(x) => f(x)
        case None    => Future.successful(None)
      }
    val res: Future[Option[Int]] = fo.flatMap(repack(_, f2))
  }

  "mapping and sequencing" in {

    val of: Option[Future[Int]] = Some(Future.successful(1))

    val fo: Future[Option[Int]] = of.sequence
    val fa: Future[Either[Throwable, Option[Int]]] = fo.attempt
    println(fa)

    val fl: Future[List[Int]] = Future.successful(List(1, 2))
    "val lf = fl.sequence" mustNot compile
    // there is no instance providing a 'sequence' for Future[Option] in cats
  }

}
