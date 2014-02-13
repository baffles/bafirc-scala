import sbt._
import Keys._

object BuildDef extends Build {
	lazy val bafirc_io = Project("bafirc-io", file("bafirc-io"))
		.settings(
	    	organization := "cc.baf",
			version := "1.0",
			scalacOptions := List("-deprecation", "-unchecked", "-feature")
		).settings(
			resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",

			libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3",
	    	libraryDependencies += "org.parboiled" %% "parboiled-scala" % "1.1.5",

	    	libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
		)
}