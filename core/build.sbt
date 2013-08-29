name := "f1lesystem-core"

description := "Scala file-system abstractions"

organization := "org.alexboisvert"

scalaVersion := "2.9.3"

libraryDependencies ++= Seq(
  "junit" % "junit" % "[4.0,)" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

