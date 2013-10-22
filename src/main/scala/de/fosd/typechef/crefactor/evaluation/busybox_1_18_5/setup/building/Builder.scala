package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.setup.building

import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.BusyBoxEvaluation
import de.fosd.typechef.parser.c.AST
import java.io.File

object Builder extends BusyBoxEvaluation {

    def canBuild(ast: AST, file: String) = {
        val currentFile = new File(file)

        // clean dir first
        runScript("./cleanAndReset.sh", busyBoxPath)
        val refFile = new File(currentFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + currentFile.getName)
        val resultDir = new File(currentFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/")

        resultDir.mkdirs()

        // write AST in current result dir
        writeAST(ast, refFile.getCanonicalPath)
        writeAST(ast, currentFile.getCanonicalPath)

        def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
            val buildResult = build
            val testResult = runTest
            writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + ext + ".build")
            if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + ext + ".buildErr")
            writeResult(testResult, resultDir.getCanonicalPath + "/" + ext + ".test")
            busyBoxFile.delete()
            (buildResult._1, testResult)
        }

        val refTest = buildAndTest(currentFile, "_ppp")

        // clean dir
        runScript("./cleanAndReset.sh", busyBoxPath)

        val orgTest = buildAndTest(currentFile, "_org")

        // clean dir
        runScript("./cleanAndReset.sh", busyBoxPath)

        val canBuildAndTest = (orgTest._1 && refTest._1) && (orgTest._2.equals(refTest._2))
        writeResult(canBuildAndTest.toString, resultDir.getCanonicalPath + "/result")
        if (!canBuildAndTest) writeResult("Fail", resultDir.getCanonicalPath + "/test.error")

        canBuildAndTest
    }

    private def runTest: String = {
        val result = runScript("./runtest", busyBoxPath + "testsuite/")
        val stream = streamsToString(result)
        stream._1
    }

    private def build: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", busyBoxPath)
        val stream = streamsToString(result)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

}
