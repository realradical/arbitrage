val Http4sVersion = "0.21.1"
val CirceVersion = "0.13.0"
val MonixVersion = "3.1.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.jacobshao",
    name := "arbitrage",
    version := "0.1",
    scalaVersion := "2.13.2",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "io.circe" %% "circe-generic-extras" % CirceVersion,
      "io.monix" %% "monix" % MonixVersion,
    ),
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Ywarn-numeric-widen",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:stars-align",
  "-Xlint:constant",
  "-Xlint:adapted-args"
)