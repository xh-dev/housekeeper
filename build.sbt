import sbtassembly.MergeStrategy

name := "housekeeper"

version := "0.0.2"

scalaVersion := "3.1.1"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"
libraryDependencies += "me.xethh.utils" % "DateUtils" % "6.0.0.RC9"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.36"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.36"

assembly / mainClass := Some("housekeeper.Main")
assembly / assemblyJarName := "housekeeper.jar"
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case _ => MergeStrategy.last
}