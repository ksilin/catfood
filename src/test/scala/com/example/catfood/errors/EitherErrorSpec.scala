package com.example.catfood.errors

import com.example.catfood.errors.ApplicationDomain._
import org.scalatest.{ FreeSpec, MustMatchers }

class EitherErrorSpec extends FreeSpec with MustMatchers {

  def arm: Either[SystemOffline, Nuke]      = Right(Nuke())
  def aim: Either[RotationNeedsOil, Target] = Right(Target())
  def launch(target: Target, nuke: Nuke): Either[MissedByMeters, Impacted] =
    Left(MissedByMeters(5))

  def attack(): Either[NukeException, Impacted] =
    for {
      nuke     <- arm
      target   <- aim
      impacted <- launch(target, nuke)
    } yield impacted

  "must attack" in {
    attack() mustBe Left(MissedByMeters(5))
  }

}
