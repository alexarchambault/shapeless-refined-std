import SonatypeKeys._

organization := "com.github.alexarchambault"

name := "shapeless-refined-std"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest" % "2.2.0" % "test"
)

libraryDependencies ++= {
  if (scalaVersion.value startsWith "2.10.")
    Seq(
      "com.chuusai" %% "shapeless" % "2.1.0-RC1" cross CrossVersion.full,
      compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )
  else
    Seq(
      "com.chuusai" %% "shapeless" % "2.1.0-RC1"
    )
}

Boilerplate.settings

Boilerplate.boilerplateSpecialToken in Boilerplate.boilerplateGenerate := "$"

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

