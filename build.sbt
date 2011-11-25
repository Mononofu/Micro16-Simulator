import AssemblyKeys._ // put this at the top of the file

seq(assemblySettings: _*)

name := "micro16"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }
