name := "sbnn"

version := "0.1"

scalaVersion := "2.12.10"

idePackagePrefix := Some("sbnn")

libraryDependencies += "io.circe" %% "circe-parser" % "0.14.1"
// https://mvnrepository.com/artifact/io.circe/circe-core
libraryDependencies += "io.circe" %% "circe-core" % "0.14.1"
// https://mvnrepository.com/artifact/io.circe/circe-generic
libraryDependencies += "io.circe" %% "circe-generic" % "0.14.1"
// https://mvnrepository.com/artifact/io.circe/circe-derivation
libraryDependencies += "io.circe" %% "circe-derivation" % "0.13.0-M5"