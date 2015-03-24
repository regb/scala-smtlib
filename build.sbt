name := "Scala SMT-LIB"

version := "0.1"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.10.4", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

javaOptions in IntegrationTest ++= Seq("-Xss10M")

fork in IntegrationTest := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test,it"

logBuffered in IntegrationTest := false

parallelExecution in Test := true
