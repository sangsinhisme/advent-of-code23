ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "freelancer",
    libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test,
    libraryDependencies += "org.jsoup" % "jsoup" % "1.14.3",
    libraryDependencies += "com.typesafe" % "config" % "1.4.3"

  )
