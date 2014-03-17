name := "Akka Sobel"

version := "1.0"
 
scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

