package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.setup.building

import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.BusyBoxEvaluation
import de.fosd.typechef.parser.c.AST
import java.io.File

object Builder extends BusyBoxEvaluation {

    def canBuild(ast: AST, file: String) = {
        val run = 0
        val currentFile = new File(file)

        // write AST
        val dir = getResultDir(currentFile.getCanonicalPath, 0)
        val path = dir.getCanonicalPath + File.separatorChar + getFileName(currentFile.getCanonicalPath)
        writeAST(ast, path)

        val workingPath = currentFile.getCanonicalPath
        val orgFile = new File(currentFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched"))
        val refFile = new File(currentFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + currentFile.getName)
        val resultDir = new File(currentFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/")

        def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
            val buildResult = build
            val testResult = runTest
            writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + ext + ".build")
            if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + ext + ".buildErr")
            writeResult(testResult, resultDir.getCanonicalPath + "/" + ext + ".test")
            busyBoxFile.delete()
            (buildResult._1, testResult)
        }

        // clean dir first
        runScript("./buildClean.sh", busyBoxPath)

        val orgTest = buildAndTest(currentFile, "_org")

        val buildRefFile = new File(workingPath)

        // clean dir first
        runScript("./buildClean.sh", busyBoxPath)

        // Replace original file with refactored file
        copyFile(refFile, buildRefFile)

        val refTest = buildAndTest(buildRefFile, "_ref")

        // Restore old original file again
        copyFile(orgFile, new File(workingPath))

        orgTest._1 == refTest._1
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
