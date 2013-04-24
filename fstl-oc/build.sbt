name := "fstl-oc"

version := "0.5"

scalaVersion := "2.10.1"

organization := "net.szumo"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "com.twitter" % "util-core_2.10" % "6.1.0" % "test",
  "com.logentries.re2" % "libre2-java" % "1.0" % "test"
)

publishMavenStyle := true

publishTo <<= (version) { version: String =>
	val repoInfo = ( "net.szumo repository" -> "/www/maven/" )
	val user = System.getProperty("user.name")
 	val keyFile = (Path.userHome / ".ssh" / "id_rsa").asFile
	Some( Resolver.ssh( repoInfo._1, "sentry.szumo.net", repoInfo._2 ) as ( user, keyFile ) withPermissions( "0644" ) )
}

