name := "fstl-oc"

version := "SNAPSHOT"

scalaVersion := "2.10.1"

organization := "net.szumo"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)
