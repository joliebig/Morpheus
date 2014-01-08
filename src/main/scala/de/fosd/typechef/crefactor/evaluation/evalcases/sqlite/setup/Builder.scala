package de.fosd.typechef.crefactor.evaluation.sqlite.setup

import de.fosd.typechef.crefactor.evaluation.setup.Building
import de.fosd.typechef.parser.c.AST
import java.io.File
import de.fosd.typechef.crefactor.evaluation.sqlite.SQLiteEvaluation


object Builder extends SQLiteEvaluation with Building {
    def canBuild(ast: AST, file: String): Boolean = {
        val currentFile = new File(file)
        // clean dir first
        runScript("./clean.sh", sourcePath)

        val refFile = new File(currentFile.getCanonicalPath.replace(evalName, "result") + "/" + currentFile.getName)
        val resultDir = new File(currentFile.getCanonicalPath.replace(evalName, "result") + "/")
        resultDir.mkdirs()

        def buildAndTest(busyBoxFile: File, ext: String): Boolean = {
            val buildResult = build
            writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + ext + ".build")
            if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + ext + ".buildErr")
            buildResult._1
        }

        val org = buildAndTest(currentFile, "_org")

        // clean dir
        runScript("./clean.sh", sourcePath)

        // write AST in current result dir
        printAndWriteAST(ast, refFile.getCanonicalPath)
        println("+++ Saving result to: " + refFile.getPath)
        println("+++ Updating file: " + currentFile.getCanonicalPath)
        printAndWriteAST(ast, currentFile.getCanonicalPath)

        val ppp = buildAndTest(currentFile, "_ppp")

        // clean dir
        runScript("./clean.sh", sourcePath)

        ppp && org
    }


    private def build: (Boolean, String, String) = {
        println("+++ Building")
        val result = runScript("./build.sh", sourcePath)
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

}
