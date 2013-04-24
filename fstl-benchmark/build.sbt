name := "fstl-benchmark"

version := "SNAPSHOT"

scalaVersion := "2.10.1"

organization := "net.szumo"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "net.szumo" %% "fstl-ac" % "0.5",
  "com.twitter" % "util-core_2.10" % "6.1.0",
  "com.logentries.re2" % "libre2-java" % "1.0"
)

publishMavenStyle := true

publishTo <<= (version) { version: String =>
	val user = System.getProperty("user.name")
 	val keyFile = (Path.userHome / ".ssh" / "id_rsa").asFile
	Some( Resolver.ssh( "net.szumo repository", "sentry.szumo.net", "/www/maven/" ) as ( user, keyFile ) withPermissions( "0644" ) )
}

