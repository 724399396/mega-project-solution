scalaVersion := "2.11.8"

name := "Mega Project"

// Typesafe snapshots
resolvers += "Typesafe Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  "org.scalaj" %% "scalaj-http" % "2.3.0",
  "com.typesafe.play" % "play-json_2.11" % "2.5.9",
  "org.scalaz" %% "scalaz-core" % "7.2.6"
)
