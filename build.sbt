name := "gieter"

version := "0.1.0"

scalaVersion := "2.12.1"

enablePlugins(JavaAppPackaging)
mainClass in Compile := Some("it.flatmap.gieter.Committer")
