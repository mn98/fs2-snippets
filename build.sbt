name := "fs2-snippets"

version := "0.1"

scalaVersion := "3.3.1"

val fs2Version = "3.9.3"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % fs2Version,
  "co.fs2" %% "fs2-io" % fs2Version,
  "co.fs2" %% "fs2-reactive-streams" % fs2Version
)
