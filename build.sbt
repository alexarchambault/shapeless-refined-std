organization := "com.github.alexarchambault"

name := "shapeless-refined-std"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest" % "2.2.0" % "test"
)

libraryDependencies ++= {
  if (scalaVersion.value startsWith "2.10.")
    Seq(
      "com.chuusai" %% "shapeless" % "2.1.0" cross CrossVersion.full,
      compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
    )
  else
    Seq(
      "com.chuusai" %% "shapeless" % "2.1.0"
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

licenses := Seq("Apache 2.0" -> url("http://opensource.org/licenses/Apache-2.0"))

pomExtra := {
  <url>https://github.com/alexarchambault/shapeless-refined-std</url>
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

credentials += {
  Seq("SONATYPE_USER", "SONATYPE_PASS").map(sys.env.get) match {
    case Seq(Some(user), Some(pass)) =>
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    case _ =>
      Credentials(Path.userHome / ".ivy2" / ".credentials")
  }
}

releaseSettings

ReleaseKeys.versionBump := sbtrelease.Version.Bump.Bugfix

sbtrelease.ReleasePlugin.ReleaseKeys.publishArtifactsAction := PgpKeys.publishSigned.value

scalacOptions += "-target:jvm-1.7"

