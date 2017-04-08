package com.example.catfood.errors

import cats._
import com.example.catfood.errors.ApplicationDomain._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.{ FreeSpec, MustMatchers }

class MonadErrorSpec extends FreeSpec with MustMatchers with ApplicationLogic {

  // now we can use any monad or a combination of monads
  // this is a solution for the propagation of result types - localize changes in a MonadError implicit

  type Result[A] = Future[Either[NukeException, A]]

  implicit val resultMonadError: MonadError[Result, NukeException] =
    new MonadError[Result, NukeException] {

      override def raiseError[A](e: NukeException): Result[A] = Future.successful(Left(e))

      override def handleErrorWith[A](fa: Result[A])(f: (NukeException) => Result[A]): Result[A] =
        fa flatMap {
          case Right(x: A) => pure(x)
          case Left(ex)    => f(ex)
        }

      override def flatMap[A, B](fa: Result[A])(f: (A) => Result[B]): Result[B] = fa flatMap {
        case Right(x: A) => f(x)
        case Left(ex)    => raiseError(ex)
      }

      override def tailRecM[A, B](a: A)(f: (A) => Result[Either[A, B]]): Result[B] = f(a) flatMap {
        case Left(ex) => raiseError(ex)
        case Right(c) =>
          c match {
            case Left(cont) => tailRecM(cont)(f)
            case Right(x)   => pure(x)
          }
      }

      override def pure[A](x: A): Result[A] = Future.successful(Right(x))

      override def ap[A, B](ff: Result[A => B])(fa: Result[A]): Result[B] = {
        val zipped: Future[(Either[NukeException, A], Either[NukeException, (A) => B])] =
          fa.zip(ff)
        zipped map {
          case (a: Either[NukeException, A], f: Either[NukeException, (A) => B]) =>
            for {
              // TODO - why is there a compiler error again? Is this about projections?
              // Error:(70, 33) value withFilter is not a member of scala.util.Either[com.example.catfood.errors.Domain.NukeException,A => B]
//              fr: ((A) => B) <- f
//              Error:(71, 33) value withFilter is not a member of scala.util.Either[com.example.catfood.errors.Domain.NukeException,A]
//              ar: A          <- a
              fr <- f
              ar <- a
            } yield fr(ar)
        }

      }
    }

  "must run" in {
    val getResult: Result[Impacted] = attack[Result]
    val res                         = Await.result(getResult, 10.seconds)
    res mustBe Left(MissedByMeters(100))
  }
}
