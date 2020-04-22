name := "cats-sandbox"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.1"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3"

// scalac options come from the sbt-tpolecat plugin so need to set any here

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
