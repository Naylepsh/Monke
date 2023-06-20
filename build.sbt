val scala3Version = "3.3.0"

val munitVersion = "0.7.29"

lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "monke",
    version                                := "0.1.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
  )
