val scala3Version = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "student-debt-calculator",
    version := "0.1.0",

    scalaVersion := scala3Version,

    Global / onChangedBuildSource := ReloadOnSourceChanges,

    libraryDependencies ++= deps,
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val deps = Seq(
  "org.scalameta" % "munit_3.0.0-M3" % "0.7.21" % Test
)