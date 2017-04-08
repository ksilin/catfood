package com.example.catfood.errors

import cats._
import cats.implicits._
import com.example.catfood.errors.ApplicationDomain._

trait ApplicationLogic {

  // trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F]
  // trait ApplicativeError[F[_], E] extends Applicative[F]
  type NukeMonadError[M[_]] = MonadError[M, NukeException]

  def arm[M[_]: NukeMonadError]: M[Nuke]   = Nuke().pure[M]
  def aim[M[_]: NukeMonadError]: M[Target] = Target().pure[M]
  def launch[M[_]: NukeMonadError](nuke: Nuke, target: Target): M[Impacted] =
    (MissedByMeters(100): NukeException).raiseError[M, Impacted] //Impacted().pure[M]

  def attack[M[_]: NukeMonadError]: M[Impacted] = {
    val tupled: M[(Nuke, Target)] = (arm[M] |@| aim[M]).tupled
    tupled.flatMap { t: (Nuke, Target) =>
      val tupleToM: ((Nuke, Target)) => M[Impacted] = (launch[M] _).tupled
      tupleToM(t)
    }
  }

}
