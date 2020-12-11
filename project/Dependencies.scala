import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0"

  lazy val monix = "io.monix" %% "monix" % "3.3.0"

  lazy val googleApiClient = "com.google.api-client" % "google-api-client" % "1.23.0"
  lazy val googleOAuthClient = "com.google.oauth-client" % "google-oauth-client-jetty" % "1.23.0"
  lazy val googleApisGmail = "com.google.apis" % "google-api-services-gmail" % "v1-rev83-1.23.0"

  lazy val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  lazy val fastparse = "com.lihaoyi" %% "fastparse" % "2.2.2"

  lazy val monixKafka = "io.monix" %% "monix-kafka-1x" % "1.0.0-RC6"
}
