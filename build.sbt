name := "scala-prolog"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies += "org.scala-lang.virtualized" % "jline" % "latest.release"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.4" % "test"
libraryDependencies += "org.specs2" %% "specs2-matcher-extra" % "3.8.4" % "test"
