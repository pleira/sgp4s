name := """sgp4s"""

version := "0.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq(
  "-deprecation", 
  "-encoding", "UTF-8",
  "-feature", 
  "-unchecked",
  "-language:implicitConversions"
)

libraryDependencies ++= Seq(
   "org.scalatest" %% "scalatest" % "2.0" % "test",
   "org.spire-math" %% "spire" % "0.7.4"
)

// for the repl
initialCommands :=
  """
import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions
  """
