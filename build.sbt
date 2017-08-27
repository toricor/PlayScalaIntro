name := "play-scala-intro"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-slick" % "2.0.2",
  "com.typesafe.play" %% "play-slick-evolutions" % "2.0.2",
//  "com.h2database" % "h2" % "1.4.190",
  specs2 % Test,
  filters,
  "mysql" % "mysql-connector-java" % "5.1.36",
  "com.adrianhurt" %% "play-bootstrap3" % "0.4.5-P24"
)

