val thisScalaVersion = "2.12.8"

val root = Project("root", file("."))
  .settings(
    name := "intermediate_wk3",
    organization := "com.example",
    version := "0.1.0",
    scalaVersion := thisScalaVersion,
    /* crossScalaVersions := Seq("2.11.11", thisScalaVersion), */
    scalacOptions ++= Seq("-language:higherKinds", "-Ypartial-unification"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.6.0",

      "org.scalatest" %% "scalatest" % "3.0.7" % "test",
    )
  )
