package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.building

import de.fosd.typechef.parser.c.TranslationUnit
import java.io.File
import de.fosd.typechef.crefactor.evaluation.setup.Building
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.BusyBoxEvaluation
import de.fosd.typechef.featureexpr.FeatureModel

object Builder extends BusyBoxEvaluation with Building {

    def canBuild(tunit: TranslationUnit, fm: FeatureModel, file: String): Boolean = {
        val currentFile = new File(file)

        // clean dir first
        runScript("./cleanAndReset.sh", sourcePath)
        val refFile = new File(currentFile.getCanonicalPath.replace(evalName, "result") + "/" + currentFile.getName)
        val resultDir = new File(currentFile.getCanonicalPath.replace(evalName, "result") + "/")

        resultDir.mkdirs()

        // write AST in current result dir
        writePrettyPrintedTUnit(tunit, refFile.getCanonicalPath.replace(".pi", ".c"))
        writePrettyPrintedTUnit(tunit, currentFile.getCanonicalPath)

        def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
            val buildResult = build
            val testResult = runTest
            writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + ext + ".build")
            if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + ext + ".buildErr")
            writeResult(testResult, resultDir.getCanonicalPath + "/" + ext + ".test")
            (buildResult._1, testResult)
        }

        val refTest = buildAndTest(currentFile, "_ppp")

        // clean dir
        runScript("./cleanAndReset.sh", sourcePath)

        val orgTest = buildAndTest(currentFile, "_org")

        // clean dir
        runScript("./cleanAndReset.sh", sourcePath)

        val canBuildAndTest = (orgTest._1 && refTest._1) && (orgTest._2.equals(refTest._2))
        writeResult(canBuildAndTest.toString, resultDir.getCanonicalPath + "/result")
        if (!canBuildAndTest) writeResult("Fail", resultDir.getCanonicalPath + "/test.error")

        canBuildAndTest
    }

    private def runTest: String = {
        val result = runScript("./runtest", sourcePath + "testsuite/")
        val stream = streamsToString(result)
        stream._1
    }

    private def build: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", sourcePath)
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

}
