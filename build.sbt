name := "Scala SMT-LIB"

version := "0.1"

scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.10.4", "2.11.2", "2.11.3", "2.11.4", "2.11.5")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
