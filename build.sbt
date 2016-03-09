organization := "com.regblanc"

name := "scala-smtlib"

version := "0.2"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.4", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

javaOptions in IntegrationTest ++= Seq("-Xss10M")

fork in IntegrationTest := true

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test,it"

logBuffered in IntegrationTest := false

parallelExecution in Test := true



publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if(isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://jsuereth.com/scala-arm</url>
    <licenses>
      <license>
        <name>MIT-style</name>
        <url>https://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:regb/scala-smtlib.git</url>
      <connection>scm:git:git@github.com:regb/scala-smtlib.git</connection>
    </scm>
    <developers>
      <developer>
        <id>reg</id>
        <name>Regis Blanc</name>
        <url>http://regblanc.com</url>
      </developer>
    </developers>
)
