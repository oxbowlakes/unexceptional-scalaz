lazy val `unexceptional-scalaz` = (project in file(".")).
  settings(
    name := "unexceptional-scalaz",
    version := "1.0",
    scalaVersion := "2.11.6",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core"     % "7.1.+",
      "org.scalaz" %% "scalaz-effect"     % "7.1.+"
    )
  )