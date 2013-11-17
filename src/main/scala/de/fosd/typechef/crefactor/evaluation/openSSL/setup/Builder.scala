package de.fosd.typechef.crefactor.evaluation.openSSL.setup

import de.fosd.typechef.crefactor.evaluation.openSSL.OpenSSLEvaluation
import de.fosd.typechef.crefactor.evaluation.setup.Building
import de.fosd.typechef.parser.c.AST
import java.io.File


object Builder extends OpenSSLEvaluation with Building {
    def canBuild(ast: AST, file: String): Boolean = {
        val currentFile = new File(file)

        // clean dir first
        runScript("./clean.sh", sourcePath)
        val refFile = new File(currentFile.getCanonicalPath.replaceAll(evalName, "result") + "/" + currentFile.getName)
        val resultDir = new File(currentFile.getCanonicalPath.replaceAll(evalName, "result") + "/")

        // write AST in current result dir
        printAndWriteAST(ast, refFile.getCanonicalPath.replace(".pi", ".c"))
        printAndWriteAST(ast, currentFile.getCanonicalPath)

        resultDir.mkdirs()

        def buildAndTest(busyBoxFile: File, ext: String): Boolean = {
            val buildResult = build
            val testResult = test
            writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + ext + ".build")
            if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + ext + ".buildErr")
            writeResult(testResult._2 + "\n" + testResult._3, resultDir.getCanonicalPath + "/" + ext + ".test")
            buildResult._1 && testResult._1
        }

        val ppp = buildAndTest(currentFile, "_ppp")

        // clean dir
        runScript("./clean.sh", sourcePath)

        val org = buildAndTest(currentFile, "_org")

        // clean dir
        runScript("./clean.sh", sourcePath)

        ppp && org
    }


    private def build: (Boolean, String, String) = {
        val result = runScript("./build.sh", sourcePath)
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

    private def test: (Boolean, String, String) = {
        val result = runScript("./test.sh", sourcePath)
        val stream = streamsToString(result)
        println("+++ Testing")
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Test"), stream._1, stream._2)
    }
}
