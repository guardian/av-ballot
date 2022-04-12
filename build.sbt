name := "av-ballot"

description := "Processing AV ballot votes"

scalaVersion := "3.1.1"

libraryDependencies ++= Seq(
  ("com.nrinaudo" %% "kantan.csv" % "0.6.2").cross(CrossVersion.for3Use2_13),
  "com.github.scopt" %% "scopt" % "4.0.1",
  "org.typelevel" %% "cats-core" % "2.7.0",
  "com.madgag" %% "scala-collection-plus" % "0.11",
  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)
