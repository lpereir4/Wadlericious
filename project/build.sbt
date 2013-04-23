
resolvers ++= Seq(
  ("sonatype.repo" at "https://oss.sonatype.org/content/groups/public"),
  ("sbt-idea-repo" at "http://mpeltonen.github.com/maven/"),
  Classpaths.typesafeResolver
)

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0")

