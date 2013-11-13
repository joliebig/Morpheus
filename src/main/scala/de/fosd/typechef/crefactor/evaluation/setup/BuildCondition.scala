package de.fosd.typechef.crefactor.evaluation.setup

import java.io.{File, FileWriter}

trait BuildCondition {

    private val buildPrefixes = List("//applet:", "//kbuild:", "//config:", "//usage:")

    def writeBuildCondition(file: String) = {
        val lines = scala.io.Source.fromFile(file).getLines()
        val buildConditions = lines.filter(line => buildPrefixes.exists(line.trim.startsWith)).toList
        if (!buildConditions.isEmpty) {
            val writer = new FileWriter(file.replace(".c", ".bc"), false)
            buildConditions.foreach(cond => writer.write(cond + "\n"))
            writer.flush
            writer.close
        }
    }

    def addBuildCondition(file: String, print: String = ""): String = {
        val bcFile = new File(file.replace(".c", ".bc"))
        if (bcFile.exists) scala.io.Source.fromFile(bcFile).mkString + "\n" + print
        else print
    }
}
