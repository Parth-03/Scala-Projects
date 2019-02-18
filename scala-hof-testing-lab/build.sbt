resolvers += "PLASMA" at "https://dl.bintray.com/plasma-umass/maven"
libraryDependencies += "edu.umass.cs" %% "hof-testing-lab" % "1.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
scalaVersion := "2.12.4"
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings"
  )

