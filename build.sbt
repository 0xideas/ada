
lazy val root = (project.
  in(file("."))).
  aggregate(core).
  dependsOn(core, priv, appl).
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)

Compile / unmanagedSourceDirectories += baseDirectory.value / "ada-core"
scalaSource in Compile := baseDirectory.value / "ada-priv/"

//Compile / sourceDirectories += baseDirectory.value / "ada-appl/"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oS")

//scalaJSUseMainModuleInitializer := true
//enablePlugins(ScalaJSPlugin)

lazy val commonSettings = Seq(
  scalaVersion := "2.13.4",
  name := "AdaEnsemble",
  version := "0.1",
  organization := "com.github.leontl"
)

lazy val core = (project.
  in(file("ada-core"))).
  settings(name := "ada-core").
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)

lazy val priv = (project.
  in(file("ada-priv"))).
  dependsOn(core).
  settings(name := "ada-priv").
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)

lazy val appl = (project.
  in(file("ada-appl"))).
  dependsOn(core, priv).
  settings(name := "ada-appl").
  settings(commonSettings).
  settings(libraryDependencies ++= dependencies)


//resolvers += Resolver.bintrayRepo("rainier", "maven")

val breezeVersion = "1.1"
val circeVersion = "0.12.3"
val log4jversion = "2.13.3"

lazy val dependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.clapper" %% "grizzled-slf4j" % "1.3.4",
  //"com.stripe" %% "rainier-core" % "0.3.3",

  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion,

  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  "org.apache.logging.log4j" % "log4j-core" % log4jversion,
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jversion,
  
  "org.bytedeco" % "javacpp"   % "1.5.4" classifier "linux-x86_64",
  "org.bytedeco" % "openblas"  % "0.3.10-1.5.4" classifier "linux-x86_64",
  "org.bytedeco" % "arpack-ng" % "3.7.0-1.5.4" classifier "linux-x86_64",

  "com.microsoft.onnxruntime" % "onnxruntime" % "1.5.2"
)

scalaSource in Test := baseDirectory.value / "ada-appl/src/test/scala"


testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1")
mainClass in (Compile, run) := Some("ada.demos.Main")

