enablePlugins(GitVersioning)

git.useGitDescribe := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

/** The configuration of the integration tests, in `src/it`. It replaces the one of sbt, which is deprecated
 *  (sbt recommends a separate subproject instead), with the same name and id, so that `it:test` and
 *  `IntegrationTest/test` run the tests. It cannot be called `IntegrationTest` itself, as the settings below
 *  would then refer to it ambiguously.
 */
lazy val ItTest = Configuration.of("IntegrationTest", "it").extend(Runtime)

ItTest / javaOptions ++= Seq("-Xss128M")

ItTest / fork := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test;it"


ItTest / logBuffered := false

Test / parallelExecution := true

val laraOrganization = "ch.epfl.lara"

lazy val commonSettings = Seq(
  // Published to Maven Central under the namespace of the lab (it was com.regblanc for Scala 2)
  organization := laraOrganization,
  name := "scala-smtlib",
  // The standard Scala 3.9 (LTS), so that the published library works with Scala 3.9 and later,
  // including the Scala 3 compiler of the lab that Inox and Stainless use
  scalaVersion := "3.9.0",
)

lazy val root = (project in file(".")).
  configs(ItTest).
  settings(commonSettings: _*).
  settings(inConfig(ItTest)(Defaults.testSettings): _*)

// Publishing to Maven Central (see .github/workflows/release.yml). The version comes from the
// latest tag, through `git describe` (sbt-git): v0.3.0 gives 0.3.0, and -SNAPSHOT is appended when
// the working tree has uncommitted changes. `sbt publishSigned` signs the artifacts and stages
// them locally, `sbt sonaRelease` uploads them to the Central Portal and releases them (`sbt
// sonaUpload` only uploads them, to inspect them in the Portal first). Both read the Portal token
// from SONATYPE_USERNAME and SONATYPE_PASSWORD.
description := "A Scala library to interact with SMT solvers through the SMT-LIB standard"

homepage := Some(url("https://github.com/epfl-lara/scala-smtlib"))

licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

scmInfo := Some(ScmInfo(
  url("https://github.com/epfl-lara/scala-smtlib"),
  "scm:git:git@github.com:epfl-lara/scala-smtlib.git",
))

developers := List(
  Developer("reg", "Regis Blanc", "", url("http://regblanc.com")),
  Developer("epfl-lara", "EPFL Lab for Automated Reasoning and Analysis", "lara.maven@epfl.ch", url("https://lara.epfl.ch")),
)

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

publishTo := localStaging.value

// The key that signs the artifacts, when the keyring holds several
pgpSigningKey := sys.env.get("PGP_FINGERPRINT")
