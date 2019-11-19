import java.io.File
name := "Planar-system-scala"

version := "0.1"

scalaVersion := "2.13.1"

// Add dependency on ScalaFX library
/*libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"*/
// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}


// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "13" classifier osName
)
scalacOptions += "-Ymacro-annotations"

val fs = File.separator
val ivyHome = Option(sys.props("sbt.ivy.home")).getOrElse(s"${sys.props("user.home")}${fs}.ivy2")
val fxRoot = s"$ivyHome${fs}cache${fs}org.openjfx${fs}javafx-"
val fxPaths = javaFXModules.map {m =>
  s"$fxRoot$m${fs}jars${fs}javafx-$m-11-$osName.jar"
}
javaOptions ++= Seq(
  "--module-path", fxPaths.mkString(File.pathSeparator),
  "--add-modules", "ALL-MODULE-PATH"
)

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parallel-collections
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.26"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
