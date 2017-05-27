package com.example.catfood.futures

import cats.data.{ EitherT, OptionT }
import cats.implicits._
import org.scalatest.{ FreeSpec, MustMatchers }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._

class FutureOptEitherListMonadTransformSpec extends FreeSpec with MustMatchers {

  val simpleFuture: Int => Future[Int] =
    i => Future.successful(i + 1)

  val futureOpt: Int => Future[Option[Int]] =
    i => Future.successful(Some(i * 10))

  val futureEither: Int => Future[Either[String, Int]] =
    i => Future.successful(Right(i * 10))

  val futureList: Int => Future[List[Int]] =
    i => Future.successful(List(i, i + 1))

  "must convert everything to option" in {
    // converting everything to Future[Option]
    val getRes: OptionT[Future, Int] = for {
      m1: Int <- OptionT.liftF(simpleFuture(1))
      m2: Int <- OptionT.liftF(2.pure[Future])
      m6: Int <- OptionT(Option(6).pure[Future])
      m3: Int <- OptionT(futureOpt(3))
//      m4: Int <- EitherT(futureEither(4)) // doesn to work, we really need the same type
      m4: Int <- OptionT(futureEither(4) map (e => e.toOption))
//      m5: Int <- OptionT.liftF(futureList(5))
      m5: Int <- OptionT(futureList(5) map (l => l.headOption))
    } yield m1 + m2 + m3 + m4 + m5
    val r = Await.result(getRes.value, 10.seconds)
    println(r)
  }

  "must convert everything to list" in {
    // converting everything to Future[List]
    // there is no ListT as it is nontrivial to get right: https://github.com/typelevel/cats/issues/977
    val getRes: Future[List[Int]] = for {
      m1: List[Int] <- simpleFuture(1) map (List(_))  // from Future[Int]
      m2: List[Int] <- List(2).pure[Future]           // from Int
      m3: List[Int] <- Some(3).toList.pure[Future]    // from Option[Int]
      m4: List[Int] <- futureOpt(4) map (_.toList)    // from Future[Option[Int]]
      m5: List[Int] <- futureEither(5) map (_.toList) // from Future[Either[Int]]
    } yield m1 ::: m2 ::: m3 ::: m4 ::: m5
    val r = Await.result(getRes, 10.seconds)
    println(r)
  }

  // from teh upcoming FP for mere mortals book
  "converting with a DSL" in {

    implicit class Ops[In](in: In) {
      def |>[Out](f: In => Out): Out = f(in)
    }
    def liftFutureOption[A](f: Future[Option[A]])         = OptionT(f)
    def liftFuture[A](f: Future[A])                       = OptionT.liftF(f)
    def liftOption[A](o: Option[A])                       = OptionT(o.pure[Future])
    def liftFutureEither[A](o: Future[Either[String, A]]) = OptionT(o.map(e => e.toOption))
    def liftFutureList[A](o: Future[List[A]])             = OptionT(o.map(e => e.headOption))
    def lift[A](a: A)                                     = liftOption(Some(a))

    val getRes: OptionT[Future, Int] = for {
      m1: Int <- simpleFuture(1) |> liftFuture
      m2: Int <- 2 |> lift
      m6: Int <- Option(6) |> liftOption
      m3: Int <- futureOpt(3) |> liftFutureOption
      m4: Int <- futureEither(4) |> liftFutureEither
      m5: Int <- futureList(5) |> liftFutureList
    } yield m1 + m2 + m3 + m4 + m5
    val r = Await.result(getRes.value, 10.seconds)
    r mustBe Some(79)
  }

}
