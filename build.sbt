// *****************************************************************************
// Projects
// *****************************************************************************

lazy val catfood =
  project
    .in(file("."))
    .enablePlugins(GitVersioning)
    .settings(settings)
    .settings(
      libraryDependencies ++= Seq(
        library.cats,
        library.scalaCheck % Test,
        library.scalaTest  % Test
      )
    )

// *****************************************************************************
// Library dependencies
// *****************************************************************************

lazy val library =
  new {
    object Version {
      val cats = "0.9.0"

      val scalaCheck = "1.13.4"
      val scalaTest  = "3.0.1"
    }
    val cats       = "org.typelevel"        %% "cats"       % Version.cats
    val catsFre    = "org.typelevel"        %% "cats-free"  % Version.cats
    val scalaCheck = "org.scalacheck"       %% "scalacheck" % Version.scalaCheck
    val scalaTest  = "org.scalatest"        %% "scalatest"  % Version.scalaTest
  }

// *****************************************************************************
// Settings
// *****************************************************************************

lazy val settings =
  commonSettings ++
  gitSettings

lazy val commonSettings =
  Seq(
    // scalaVersion and crossScalaVersions from .travis.yml via sbt-travisci
    // scalaVersion := "2.12.1",
    // crossScalaVersions := Seq(scalaVersion.value, "2.11.8"),
    organization := "default",
    licenses += ("Apache 2.0",
    url("http://www.apache.org/licenses/LICENSE-2.0")),
    mappings.in(Compile, packageBin) += baseDirectory.in(ThisBuild).value / "LICENSE" -> "LICENSE",
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-language:_",
      "-target:jvm-1.8",
      "-encoding",
      "UTF-8"
    ),
    javacOptions ++= Seq(
      "-source",
      "1.8",
      "-target",
      "1.8"
    ),
    unmanagedSourceDirectories.in(Compile) := Seq(scalaSource.in(Compile).value),
    unmanagedSourceDirectories.in(Test) := Seq(scalaSource.in(Test).value)
  )

lazy val gitSettings =
  Seq(
    git.useGitDescribe := true
  )
