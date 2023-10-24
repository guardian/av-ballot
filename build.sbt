name := "av-ballot"

description := "Processing AV ballot votes"

scalaVersion := "3.1.3"

libraryDependencies ++= Seq(
  ("com.nrinaudo" %% "kantan.csv" % "0.7.0").cross(CrossVersion.for3Use2_13),
  "com.github.scopt" %% "scopt" % "4.1.0",
  "org.typelevel" %% "cats-core" % "2.10.0",
  "com.madgag" %% "scala-collection-plus" % "0.11",
  "com.ibm.icu" % "icu4j" % "73.2",
  "org.scalatest" %% "scalatest" % "3.2.17" % Test
)
