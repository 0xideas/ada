scalaVersion := "2.12.12"

name := "EpsilonEnsemble"
version := "0.1"
organization := "com.github.leontl"

resolvers += Resolver.bintrayRepo("rainier", "maven")

libraryDependencies  ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.14.1",
  "org.scalanlp" %% "breeze" % "1.1",
  "org.scalanlp" %% "breeze-natives" % "1.1",
  "org.scalanlp" %% "breeze-viz" % "1.1",
  //"com.stripe" %% "rainier-core" % "0.3.3",
  "com.github.haifengl" %% "smile-scala" % "2.5.3",
  "org.apache.logging.log4j" % "log4j-core" % "2.13.3",
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.13.3",
  "org.bytedeco" % "javacpp"   % "1.5.4" classifier "linux-x86_64",
  "org.bytedeco" % "openblas"  % "0.3.10-1.5.4" classifier "linux-x86_64",
  "org.bytedeco" % "arpack-ng" % "3.7.0-1.5.4" classifier "linux-x86_64"

)


testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")
