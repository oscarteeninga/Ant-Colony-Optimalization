ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "antsystem.Ant-Colony-Optimalization"
  )

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
libraryDependencies += "joda-time" % "joda-time" % "2.12.1"
libraryDependencies += "org.plotly-scala" % "plotly-render_2.13" % "0.8.1"
