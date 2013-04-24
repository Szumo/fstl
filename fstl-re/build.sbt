name := "fstl-re"

version := "0.5"

scalaVersion := "2.10.1"

organization := "net.szumo"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
)

publishMavenStyle := true

publishTo <<= (version) { version: String =>
	val user = System.getProperty("user.name")
 	val keyFile = (Path.userHome / ".ssh" / "id_rsa").asFile
	Some( Resolver.ssh( "net.szumo repository", "sentry.szumo.net", "/www/maven/" ) as ( user, keyFile ) withPermissions( "0644" ) )
}

