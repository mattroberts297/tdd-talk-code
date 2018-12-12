/**
  * build.sbt
  */

lazy val akkaHttpVersion = "10.1.5"
lazy val catsVersion = "1.4.0"
lazy val catsEffectVersion = "1.0.0"
lazy val circeVersion = "0.10.1"

lazy val akkaVersion = "2.5.4"
lazy val akkaDynamoVersion = "1.1.0"
lazy val logbackVersion = "1.2.3"
lazy val scalaTestVersion = "3.0.5"
lazy val jwtVersion = "3.4.1"
lazy val scanamoVersion = "1.0.0-M8"

lazy val akkaDependencies = Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
)

lazy val catsDependencies = Seq(
  "org.typelevel" %% "cats-core"   % catsVersion,
  "org.typelevel" %% "cats-free"   % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
)

lazy val circeDependencies = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-generic-extras"
).map(_ % circeVersion)

lazy val allDependencies = akkaDependencies ++ catsDependencies ++ circeDependencies ++ Seq(
  "ch.qos.logback"    %  "logback-classic" % logbackVersion,
  "com.auth0"         %  "java-jwt"        % jwtVersion,
  "com.gu"            %% "scanamo"         % scanamoVersion,
  "com.gu"            %% "scanamo-alpakka" % scanamoVersion
)

lazy val allTestDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion
).map(_ % "test")

lazy val allSettings = Seq(
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.7",
   // Cats needs improved type inference - see SI-2712.
  scalacOptions += "-Ypartial-unification",
  autoCompilerPlugins := true,
  libraryDependencies ++= allDependencies ++ allTestDependencies,
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
)

lazy val root = project
  .in(file("."))
  .settings(name := "type-driven-development")
  .settings(allSettings)
