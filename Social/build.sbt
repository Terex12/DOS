name := "Facbook Simulator"

version := "1.0" 

scalaVersion := "2.11.6"

resolvers += "spray repo" at "http://repo.spray.io"

val sprayVersion = "1.3.3"

libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.3.13",
	"io.spray" %%  "spray-client"  % sprayVersion,
	"io.spray" %%  "spray-can"     % sprayVersion,
    "io.spray" %%  "spray-routing" % sprayVersion,
    "io.spray" %%  "spray-testkit" % sprayVersion % "test",
	"org.json4s" %% "json4s-native" % "3.2.10"
)

