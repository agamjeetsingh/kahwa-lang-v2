ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "kahwa_lang_v2",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
