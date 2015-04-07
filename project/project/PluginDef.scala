import sbt._

object PluginDef extends Build {
  override def projects = Seq(root)

  lazy val root = Project("plugins", file(".")) dependsOn(boilerplate)

  // Trick due to @marklister
  // https://github.com/sbt/sbt-boilerplate/issues/12#issuecomment-84257889

  lazy val boilerplate = uri(
    "https://github.com/alexarchambault/sbt-boilerplate.git" +
    "#8dc9cd576b3cc75530113d9359c3e8a62b6d149f"
  )
}
