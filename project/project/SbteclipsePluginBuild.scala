import sbt._
import Keys._

object SbteclipsePluginBuild extends Build {
  lazy val sbtRelease =
    RootProject(uri("git://github.com/Atry/sbt-release.git#develop"))

  lazy val sbtscalariform =
    RootProject(uri("git://github.com/typesafehub/sbtscalariform.git#sbt-0.12"))

  lazy val sbtproperties =
    RootProject(uri("git://github.com/Atry/sbtproperties.git"))

  lazy val posterousSbt =
    RootProject(uri("git://github.com/n8han/posterous-sbt.git"))

  lazy val root =
    Project(id = "root", base = file(".")).
      dependsOn(sbtRelease, sbtscalariform, sbtproperties, posterousSbt)

}

// vim: set ts=2 sw=2 et:
