enablePlugins(GitVersioning)

git.useGitDescribe := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions ++= {
  val Seq(_, major, minor) = scalaVersion.value.split("\\.").toSeq.map(_.toInt)
  if (major <= 10 || (major == 11 && minor < 5)) Seq.empty
  else Seq("-Ypatmat-exhaust-depth", "40")
}

javaOptions in IntegrationTest ++= Seq("-Xss128M")

fork in IntegrationTest := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.4" % "test"

logBuffered in IntegrationTest := false

parallelExecution in Test := true

lazy val commonSettings = Seq(
  organization := "com.regblanc",
  name := "scala-smtlib",
  scalaVersion := "2.13.4",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.12", "2.13.4")
)

lazy val root = (project in file(".")).
  configs(IntegrationTest).
  settings(commonSettings: _*).
  settings(Defaults.itSettings: _*)

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if(version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

licenses := Seq("MIT-style" -> url("https://opensource.org/licenses/MIT"))

pomExtra := (
  <url>https://github.com/regb/scala-smtlib</url>
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
