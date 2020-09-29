
scalaVersion := "2.13.3"


sourceDirectories in Compile += file("/home/leon/projects/epsilon-ensemble/src/dataGenerators")
sourceDirectories in Compile += file("/home/leon/projects/epsilon-ensemble/src/models")
sourceDirectories in Compile += file("/home/leon/projects/epsilon-ensemble/src/main/scala")

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1"


testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")