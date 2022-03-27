lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.simonmassey",
      scalaVersion := "3.1.1"
    )),
    name := "yaaee"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

scalacOptions := Seq("-feature", "-deprecation")
