import sbt._
import Keys._

object BuildDef extends Build {
	lazy val bafirc_comm = Project("bafirc-comm", file("bafirc-comm"))
		.settings(
	    	organization := "cc.baf",
			version := "1.0",
			scalacOptions := List("-deprecation", "-unchecked", "-feature")
		).settings(
			resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",

			libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3",
	    	libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.5",

	    	libraryDependencies += "io.spray" % "spray-util" % "1.2.0",

	    	libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
		)
}