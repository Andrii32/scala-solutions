import Dependencies._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Ymacro-annotations"
)

lazy val root = (project in file("."))
  .settings(
    name := "solutions",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += simulacrum
  )

