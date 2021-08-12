name := "learning-cats"
version := "0.1"
scalaVersion := "2.13.6"
organization := "com.exabeam.rgregory"


val catsVersion = "2.3.0"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"
).map(_ % catsVersion withSources() withJavadoc())

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC5"

