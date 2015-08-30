lazy val commonSettings = Seq(
  organization := "org.example",
  version := "0.1",
  scalaVersion := "2.11.7",
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies ++= Seq(
    "org.halcat" %% "kits" % "0.5.0",
    "org.scalatest" %% "scalatest" % "2.2.5" % "test"
  ),
  scalacOptions ++= Seq("-feature", "-deprecation")
)

lazy val root = project in file(".") aggregate (core, example)

lazy val core = project in file("core") settings (commonSettings: _*)

lazy val example = project in file("example") settings (commonSettings: _*) settings (
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
) dependsOn core
