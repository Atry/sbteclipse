libraryDependencies <+= (sbtVersion) { sbtVersion =>
  val SbtVersionRegExp = """(\d+)\.(\d+)\..+"""r
  val SbtVersionRegExp(major, minor) = sbtVersion 
  if (major.toInt > 0 || minor.toInt > 11) {
    "org.scala-sbt" % "scripted-plugin" % sbtVersion
  } else {
    "org.scala-tools.sbt" %% "scripted-plugin" % sbtVersion
  }
}
