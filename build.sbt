name := "scala-prolog"

libraryDependencies += "org.scala-lang.virtualized" % "jline" % "latest.integration"

libraryDependencies += "org.specs2" %% "specs2" % "latest.integration" % "test"

autoCompilerPlugins := true
 
addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.1")
 
scalacOptions += "-P:continuations:enable"