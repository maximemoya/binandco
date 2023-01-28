ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "binandco",
    idePackagePrefix := Some("fr.maxime.binandco")
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test
