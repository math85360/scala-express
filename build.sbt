import org.scalajs.sbtplugin.cross.{ CrossProject, CrossType }

organization in ThisBuild := "com.iz2use"

val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:higherKinds",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  /*"-Yno-predef",
  "-Ywarn-unused-import"*/
)

val catsVersion = "1.1.0"
val shapelessVersion = "2.3.3"
val refinedVersion = "0.9.0"
val paradiseVersion = "2.1.1"

lazy val baseSettings = Seq(
  scalacOptions ++= compilerOptions,
  scalacOptions in (Compile, console) ~= {
    _.filterNot(Set("-Ywarn-unused-import", "-Yno-predef"))
  },
  scalacOptions in (Test, console) ~= {
    _.filterNot(Set("-Ywarn-unused-import", "-Yno-predef"))
  },
  scalacOptions in Tut ~= {
    _.filterNot(Set("-Ywarn-unused-import", "-Yno-predef"))
  },
  scalacOptions in Test ~= {
    _.filterNot(Set("-Yno-predef"))
  },
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  (scalastyleSources in Compile) ++= (unmanagedSourceDirectories in Compile).value,
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.4.3" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val allSettings = baseSettings ++ Seq()

def expressProject(path: String)(project: Project) = {
  val docName = path.split("-").mkString(" ")
  project.settings(
    description := s"express $docName",
    moduleName := s"express-$path",
    name := s"Express $docName",
    allSettings
  )
}

def expressModule(path: String): Project = {
  val id = path.split("-").reduce(_ + _.capitalize)
  Project(id, file(s"modules/$path"))
    .configure(expressProject(path))
    .settings()
}

def expressCrossModule(path: String, crossType: CrossType = CrossType.Full) = {
  val id = path.split("-").reduce(_ + _.capitalize)
  CrossProject(jvmId = id, jsId = id + "JS", file(s"modules/$path"), crossType)
    .settings(allSettings)
    .configureAll(expressProject(path))
    .jvmSettings()
}

lazy val expressJsModules = Seq[Project](coreJS)
lazy val expressJvmModules = Seq[Project](core)

lazy val expressCrossModules = Seq[(Project, Project)](
  (core, coreJS),
  (step, stepJS),
  (ifc, ifcJS)
)

lazy val jvmProjects: Seq[Project] =
  (expressCrossModules.map(_._1) ++ expressJvmModules)

lazy val jsProjects: Seq[Project] =
  (expressCrossModules.map(_._2) ++ expressJsModules)

lazy val aggregatedProjects: Seq[ProjectReference] =
  (expressCrossModules.flatMap(cp => Seq(cp._1, cp._2)) ++
   expressJsModules ++ expressJvmModules).map(p => p:ProjectReference)

lazy val macroSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    compilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.patch)
  )
)

lazy val root = project.in(file("."))
  .settings(allSettings)
  .settings(noPublishSettings)
  .settings(
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.patch),
    initialCommands in console :=
      """
        |import com.iz2use.express._
        |import com.iz2use.step._
        |import com.iz2use.ifc._
      """.stripMargin
  )
  .aggregate(aggregatedProjects: _*)

lazy val coreBase = expressCrossModule("core")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "com.chuusai" %%% "shapeless" % shapelessVersion,
      "com.chuusai" %%% "shapeless" % shapelessVersion % Test,
      "eu.timepit" %%% "refined" % refinedVersion,
      "eu.timepit" %%% "refined-scalacheck" % refinedVersion % Test
    )
  )
lazy val core = coreBase.jvm
lazy val coreJS = coreBase.js

lazy val stepBase = expressCrossModule("step")
  .settings()
  .dependsOn(coreBase)
lazy val step = stepBase.jvm
lazy val stepJS = stepBase.js

lazy val ifcBase = expressCrossModule("ifc")
  .settings(
    libraryDependencies += "org.typelevel" %%% "squants" % "1.3.0"
  )
  .dependsOn(stepBase, stepBase % Test)
lazy val ifc = ifcBase.jvm
lazy val ifcJS = ifcBase.js

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)