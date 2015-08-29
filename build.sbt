lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.5" % "test",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ),
  scalacOptions += "-deprecation"
)

lazy val root = project in file(".") aggregate (core, example)

lazy val core = project in file("core") settings (commonSettings: _*)

lazy val example = project in file("example") settings (commonSettings: _*) dependsOn core
