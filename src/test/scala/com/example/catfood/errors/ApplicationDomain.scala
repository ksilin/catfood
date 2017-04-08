package com.example.catfood.errors

object ApplicationDomain {

  case class Nuke()
  case class Target()
  case class Impacted()

  sealed trait NukeException
  case class SystemOffline()             extends NukeException
  case class RotationNeedsOil()          extends NukeException
  case class MissedByMeters(meters: Int) extends NukeException

}
