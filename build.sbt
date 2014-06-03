name := "Morpheus"

version := "1.0"

organization := "de.fosd.typechef"

scalaVersion := "2.10.2"

libraryDependencies += "de.fosd.typechef" % "frontend_2.10" % "0.3.6"

libraryDependencies += "de.fosd.typechef" % "sampling_2.10" % "0.3.6"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.0-beta4"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.0-beta4"

mainClass in Runtime := Some("de.fosd.typechef.crefactor.CRefactorFrontend")


//generate crefactor.sh file with full classpath
TaskKey[File]("mkrun") <<= (baseDirectory, fullClasspath in Runtime, mainClass in Runtime) map { (base, cp, main) =>
  val template = """#!/bin/sh
java -ea -Xmx2048M -Xss256M -XX:PermSize=512M -XX:MaxPermSize=1024M  -classpath "%s" %s "$@"
"""
  val mainStr = main getOrElse error("No main class specified")
  val contents = template.format(cp.files.absString, mainStr)
  val out = base / "../cRefactor.sh"
  IO.write(out, contents)
  out.setExecutable(true)
  out
}

