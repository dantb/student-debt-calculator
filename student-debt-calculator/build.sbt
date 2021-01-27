val scala3Version = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "student-debt-calculator",
    version := "0.1.0",

    scalaVersion := scala3Version,

    Global / onChangedBuildSource := ReloadOnSourceChanges,

    libraryDependencies ++= deps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )

lazy val deps = Seq(
  "com.disneystreaming" %% "weaver-cats" % "0.7.0-M6" % Test
)
