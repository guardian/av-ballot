name := "av-ballot"

description := "Processing AV ballot votes"

scalaVersion := "3.3.6"

libraryDependencies ++= Seq(
  ("com.nrinaudo" %% "kantan.csv" % "0.8.0").cross(CrossVersion.for3Use2_13),
  "com.github.scopt" %% "scopt" % "4.1.0",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "com.madgag" %% "scala-collection-plus" % "1.0.0",
  "com.ibm.icu" % "icu4j" % "77.1",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
