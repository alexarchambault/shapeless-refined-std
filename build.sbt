import SonatypeKeys._

import _root_.sbtbuildinfo.Plugin.BuildInfoKey

import com.typesafe.sbt.SbtGit.GitKeys._

name := "shapeless-refined-std"

organization := "com.github.alexarchambault"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases")
, Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai"    %% "shapeless" % "2.1.0-SNAPSHOT" changing()
, "org.scalatest"  %% "scalatest" % "2.2.0" % "test"
)

Boilerplate.settings

Boilerplate.boilerplateSpecialToken in Boilerplate.boilerplateGenerate := "$"

typelevelDefaultSettings

TypelevelKeys.githubProject := ("alexarchambault", name.value)

net.virtualvoid.sbt.graph.Plugin.graphSettings

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](
  name
, version
, scalaVersion
, sbtVersion
, gitHeadCommit
, BuildInfoKey.action("buildTime") {
    System.currentTimeMillis
  }
)

buildInfoPackage := "shapeless.refinedstd"

profileName := "alexandre.archambault"

xerial.sbt.Sonatype.sonatypeSettings

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := {
  <url>https://github.com/alexarchambault/shapeless-refined-std</url>
  <licenses>
    <license>
      <name>Apache 2.0</name>
      <url>http://opensource.org/licenses/Apache-2.0</url>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:github.com/alexarchambault/shapeless-refined-std.git</connection>
    <developerConnection>scm:git:git@github.com:alexarchambault/shapeless-refined-std.git</developerConnection>
    <url>github.com/alexarchambault/shapeless-refined-std.git</url>
  </scm>
  <developers>
    <developer>
      <id>alexarchambault</id>
      <name>Alexandre Archambault</name>
      <url>https://github.com/alexarchambault</url>
    </developer>
  </developers>
}

