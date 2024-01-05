ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "freelancer",
    libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"

  )
