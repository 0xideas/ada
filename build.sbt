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
  "com.stripe" %% "rainier-core" % "0.3.3"
)


testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

