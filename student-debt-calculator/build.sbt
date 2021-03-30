val scala3Version = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "student-debt-calculator",
    version := "0.1.0",
    scalaVersion := scala3Version,
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    libraryDependencies ++= deps,
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    envFileName in ThisBuild := "config.env"
  )

lazy val deps = Seq(
  "com.google.auth" % "google-auth-library-oauth2-http" % "0.23.0",
  "io.circe" %% "circe-core" % "0.14.0-M3",
  "org.typelevel" %% "cats-effect" % "3.0-65-7c98c86",
  "com.softwaremill.sttp.client3" %% "core" % "3.1.0",
  "com.disneystreaming" %% "weaver-cats" % "0.7.0-M6" % Test
)
