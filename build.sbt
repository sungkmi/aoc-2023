val V = new {
  val Scala      = "3.3.1"
  val ScalaGroup = "3.3"

  val catsEffect = "3.5.2"

  val scribe          = "3.13.0"
  val hedgehog        = "0.10.1"
  val organiseImports = "0.6.0"
  val munitCatsEffect = "2.0.0-M4"

}

val Dependencies = new {
  lazy val aoc = Seq(
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-effect"  % V.catsEffect,
      "com.outr"        %% "scribe-slf4j" % V.scribe,
    ),
  )

  lazy val tests = Def.settings(
    libraryDependencies ++= Seq(
      "qa.hedgehog" %% "hedgehog-munit" % V.hedgehog % Test,
      "org.typelevel" %% "munit-cats-effect" % V.munitCatsEffect % Test,
    ),
    Test / fork := true,
  )
}

ThisBuild / organization := "dev.sungkm"
ThisBuild / version      := "0.0.1-SNAPSHOT"
ThisBuild / scalaVersion := V.Scala
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % V.organiseImports
ThisBuild / semanticdbEnabled := true
Compile / run / fork := true

lazy val aoc = (project in file("."))
  .settings(Dependencies.aoc)
  .settings(Dependencies.tests)
  .settings(
    name := "sungkm-advent-of-code-2023",
  )
