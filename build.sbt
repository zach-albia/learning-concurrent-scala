name := "concurrent-scala"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-Xasync"
)

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.7",
  "org.scala-lang.modules" %% "scala-async" % "1.0.0-M1",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
)