name := "PrimeSemanticSearch"

version := "1.0"

scalaVersion := "2.10.6"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"

val circeVersion = "0.7.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-generic-extras"
).map(_ % circeVersion)

libraryDependencies += "com.typesafe.play" % "play-ws_2.10" % "2.4.11"

addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
)

    