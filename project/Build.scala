import sbt._
import sbt.Keys._

import sbtunidoc.Plugin._
import sbtunidoc.Plugin.UnidocKeys._

import com.typesafe.sbt.pgp.PgpKeys._

import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import sbtbuildinfo.Plugin._

object MyBuild extends Build {

  // Dependencies
  
  lazy val spireMath = "org.spire-math" %% "spire" % "0.9.1"
  //lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.2.1"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.1.3"
  // lazy val scalaUtils = "org.scalautils" %% "scalautils" % "2.1.5"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.1"

  // Release step

  lazy val publishSignedArtifacts = ReleaseStep(
    action = st => {
      val extracted = st.extract
      val ref = extracted.get(thisProjectRef)
      extracted.runAggregated(publishSigned in Global in ref, st)
    },
    check = st => {
      // getPublishTo fails if no publish repository is set up.
      val ex = st.extract
      val ref = ex.get(thisProjectRef)
      Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
      st
    },
    enableCrossBuild = true
  )

  lazy val noPublish = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  // Settings

  override lazy val settings = super.settings ++ Seq(
    organization := "sgp4s",

    scalaVersion := "2.11.5",

    crossScalaVersions := Seq("2.11.5"),

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("http://www.pitagoral.com")),

    libraryDependencies ++= Seq(
//      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),

    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
      "-Yinline-warnings",
      "-deprecation",
      "-unchecked",
      "-optimize",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-feature"
    ),

    scalacOptions := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 10)) =>
          scalacOptions.value
        case Some((2, n)) if n >= 11 =>
          scalacOptions.value ++ Seq("-Ywarn-unused-import")
      }
    },

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value

        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.0.0")
      }
    },

    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },

    pomExtra := (
<scm>
  <url>git@github.com:pleira/sgp4s.git</url>
  <connection>scm:git:git@github.com:pleira/sgp4s.git</connection>
</scm>
<developers>
  <developer>
    <id>pleira</id>
    <name>Pablo Pita</name>
    <url>http://github.com/pleira/</url>
  </developer>
</developers>
    )
  )

  // Main

  lazy val sgp4s = Project("sgp4s", file(".")).
    aggregate(core, examples, tests, benchmark).
    settings(sgp4sSettings: _*)

  lazy val sgp4sSettings = Seq(
    name := "sgp4s-aggregate"
  ) ++ noPublish ++ releaseSettings ++ Seq(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishSignedArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )

  // Core

  lazy val core = Project("core", file("core")).
    settings(coreSettings: _*)

//  lazy val genProductTypes = TaskKey[Seq[File]]("gen-product-types",
//    "Generates several type classes for Tuple2-22.")

  lazy val coreSettings = Seq(
    name := "sgp4s",
    libraryDependencies ++= Seq(
   "org.spire-math" %% "spire" % "0.9.1",
      spireMath % "compile",
      scalaCheck % "test",
      scalaTest % "test"
    //  scalaUtils % "test"
    )
  ) ++ buildInfoSettings ++ Seq(
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := "sgp4s"
  )

  // Examples

  lazy val examples = Project("examples", file("examples")).
    settings(examplesSettings: _*).
    dependsOn(core)

  lazy val examplesSettings = Seq(
    name := "sgp4s-examples",
    libraryDependencies ++= Seq(
    )
  ) ++ noPublish

  // Tests

  lazy val tests = Project("tests", file("tests")).
    settings(testsSettings: _*).
    dependsOn(core)

  lazy val testsSettings = Seq(
    name := "sgp4s-tests",
    libraryDependencies ++= Seq(
      scalaTest % "test"
    )
  ) ++ noPublish


  // Benchmark

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(core)

  lazy val benchmarkSettings = Seq(
    name := "sgp4s-benchmark",

    // raise memory limits here if necessary
    // TODO: this doesn't seem to be working with caliper at the moment :(
  
    javaOptions in run += "-Xmx4G",

    libraryDependencies ++= Seq(
    ),

    // enable forking in run
    fork in run := true
  ) ++ noPublish

}
