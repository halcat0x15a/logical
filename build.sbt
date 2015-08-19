scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
)

scalacOptions += "-deprecation"
