resolvers in ThisBuild ++= Seq(
    Resolver.sonatypeRepo("releases"),
    sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),
    "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases",
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype OSS Release Staging" at "https://oss.sonatype.org/content/groups/staging"
    )
resolvers += Resolver.typesafeRepo("releases")
addSbtCoursier
addSbtPlugin("com.47deg" % "sbt-microsites" % "0.7.18")
addSbtPlugin("com.dwijnand" % "sbt-travisci" % "1.1.1")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.8")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.2.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.22")
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.3")