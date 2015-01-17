scalaVersion := "2.10.4"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.9.0",
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
)