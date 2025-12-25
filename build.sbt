val gol = (project in file("."))
  .settings(
    name := "game-of-life",
    version := "0.3-SNAPSHOT",
    scalaVersion := "3.3.7",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
