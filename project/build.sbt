name := """sgp4s"""

version := "0.1"

scalaVersion := "2.10.4"

scalacOptions ++= Seq(
  "-deprecation", 
  "-encoding", "UTF-8",
  "-feature", 
  "-unchecked",
  "-language:implicitConversions"
)

libraryDependencies ++= Seq(
   "org.scalatest" %% "scalatest" % "2.0" % "test",
   "org.scalaz" %% "scalaz-core" % "7.0.6",
   "org.scalaz" %% "scalaz-geo" % "6.0.4",
   "org.spire-math" %% "spire" % "0.8.2"
)

// for the repl
initialCommands :=
  """
import scalaz.contrib.geo._  
import Geo._
import spire.algebra._   // provides algebraic type classes
import spire.math._      // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions
  """
