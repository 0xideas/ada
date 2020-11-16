scalaVersion := "2.12.12"

name := "EpsilonEnsemble"
version := "0.1"
organization := "com.github.leontl"

resolvers += Resolver.bintrayRepo("rainier", "maven")

val breezeVersion = "1.1"
val circeVersion = "0.12.3"
val log4jversion = "2.13.3"

libraryDependencies  ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "com.github.haifengl" %% "smile-scala" % "2.5.3",
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
  "org.bytedeco" % "arpack-ng" % "3.7.0-1.5.4" classifier "linux-x86_64"
)


testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")
