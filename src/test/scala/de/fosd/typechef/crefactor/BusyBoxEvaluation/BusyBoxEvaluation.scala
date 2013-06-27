package de.fosd.typechef.crefactor.BusyBoxEvaluation

import java.io._
import de.fosd.typechef.featureexpr.FeatureModel
import org.junit.Test
import de.fosd.typechef.crefactor.util.EvalHelper


trait BusyBoxEvaluation extends EvalHelper {

    val FORCE_VARIABILITY = true
    val MAX_DEPTH = 27

    val amountOfRefactorings = 3

    @Test
    def evaluate()
}


object RefactorVerification extends EvalHelper {

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean = {
        val workingPath = bbFile.getCanonicalPath
        val orgFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched"))
        val refFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + bbFile.getName)
        val verfiyDir = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/")

        val configs = verfiyDir.listFiles(new FilenameFilter {
            def accept(input: File, file: String): Boolean = file.endsWith(".config")
        })

        configs.forall(config => {
            def buildAndTest(busyBoxFile: File, ext: String): String = {
                val buildResult = buildBusyBox
                val testResult = runTest
                writeResult(buildResult._2, verfiyDir.getCanonicalPath + "/" + config.getName + ext + ".buildAndTest")
                writeResult(testResult, verfiyDir.getCanonicalPath + "/" + config.getName + ext + ".test")
                bbFile.delete()
                testResult
            }

            val configBuild = new File(busyBoxPath + ".config")
            copyFile(config, configBuild)

            val orgTest = buildAndTest(bbFile, "_org")

            val buildRefFile = new File(workingPath)

            // Replace original file with refactored file
            copyFile(refFile, buildRefFile)

            val refTest = buildAndTest(buildRefFile, "_ref")

            // Restore old original file again
            copyFile(orgFile, new File(workingPath))
            configBuild.delete()

            writeResult(orgTest.equals(refTest).toString, verfiyDir.getCanonicalPath + "/" + config.getName + ".result")
            orgTest.equals(refTest)
        })
    }

    def runTest: String = {
        val result = runScript("./runtest", busyBoxPath + "testsuite/")
        val stream = streamsToString(result)
        stream._1 + "\n" + stream._2
    }

    def buildBusyBox: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", busyBoxPath)
        val stream = streamsToString(result)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

}
