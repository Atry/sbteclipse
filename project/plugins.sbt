
resolvers ++= Seq(
  Classpaths.typesafeSnapshots,
  "gseitz@github" at "http://gseitz.github.com/maven/",
  Resolver.url("heikoseeberger", new URL("http://hseeberger.github.com/releases"))(Resolver.ivyStylePatterns)
)

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.5-SNAPSHOT")

addSbtPlugin("com.typesafe.sbtscalariform" % "sbtscalariform" % "0.4.0-SNAPSHOT")

//addSbtPlugin("name.heikoseeberger.sbtproperties" % "sbtproperties" % "1.0.1")

//addSbtPlugin("net.databinder" % "posterous-sbt" % "0.3.2")

//libraryDependencies <+= (sbtVersion)(sbtVersion =>
//  "org.scala-sbt" % "scripted-plugin" % sbtVersion
//)
