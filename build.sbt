
lazy val root = (project.
  in(file("."))).
  aggregate(core).
  dependsOn(core, appl).
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)

Compile / unmanagedSourceDirectories += baseDirectory.value / "ada-core"

//Compile / sourceDirectories += baseDirectory.value / "ada-appl/"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oS")

//scalaJSUseMainModuleInitializer := true
//enablePlugins(ScalaJSPlugin)
scalacOptions ++= Seq(
    "-deprecation",
    "-feature"
)

lazy val commonSettings = Seq(
  scalaVersion := "3.0.0",
  crossScalaVersions ++= Seq("2.13.4", "3.0.0"),
  name := "AdaEnsemble",
  version := "0.1",
  organization := "com.github.leontl"
)

lazy val core = (project.
  in(file("ada-core"))).
  settings(name := "ada-core").
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)


lazy val appl = (project.
  in(file("ada-appl"))).
  dependsOn(core).
  settings(name := "ada-appl").
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)


//resolvers += Resolver.bintrayRepo("rainier", "maven")

val breezeVersion = "1.1"
val circeVersion = "0.12.3"
val log4jversion = "2.13.3"

lazy val dependencies = Seq(
  ("org.scalacheck" %% "scalacheck" % "1.14.1").cross(CrossVersion.for3Use2_13),
  ("org.clapper" %% "grizzled-slf4j" % "1.3.4").cross(CrossVersion.for3Use2_13),
  //"com.stripe" %% "rainier-core" % "0.3.3",

  ("org.scalanlp" %% "breeze" % breezeVersion).cross(CrossVersion.for3Use2_13),
  ("org.scalanlp" %% "breeze-natives" % breezeVersion).cross(CrossVersion.for3Use2_13),
  ("org.scalanlp" %% "breeze-viz" % breezeVersion).cross(CrossVersion.for3Use2_13),

  ("io.circe" %% "circe-core" % circeVersion).cross(CrossVersion.for3Use2_13),
  ("io.circe" %% "circe-generic" % circeVersion).cross(CrossVersion.for3Use2_13),
  ("io.circe" %% "circe-parser" % circeVersion).cross(CrossVersion.for3Use2_13),

  ("org.apache.logging.log4j" % "log4j-core" % log4jversion),
  ("org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jversion),
  
  ("org.bytedeco" % "javacpp"   % "1.5.4" classifier "linux-x86_64"),
  ("org.bytedeco" % "openblas"  % "0.3.10-1.5.4" classifier "linux-x86_64"),
  ("org.bytedeco" % "arpack-ng" % "3.7.0-1.5.4" classifier "linux-x86_64"),

  ("com.microsoft.onnxruntime" % "onnxruntime" % "1.5.2")
)

scalaSource in Test := baseDirectory.value / "ada-appl/src/test/scala"


testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
mainClass in (Compile, run) := Some("ada.demos.Main")

