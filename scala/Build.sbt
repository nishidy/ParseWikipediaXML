name := "ParseWikipediaXML"

version := "1.0"

scalaVersion := "2.10.3"

resolvers ++= Seq (
    Resolver.sonatypeRepo("public"),
    "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
    "com.github.scopt" %% "scopt" % "3.3.0",
    "com.typesafe.akka" %% "akka-actor" % "2.1.1"
)

excludeFilter in unmanagedSources := HiddenFileFilter || "*.java"

