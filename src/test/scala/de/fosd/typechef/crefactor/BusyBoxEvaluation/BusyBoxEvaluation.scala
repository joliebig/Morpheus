package de.fosd.typechef.crefactor.BusyBoxEvaluation

import java.io._
import de.fosd.typechef.featureexpr.FeatureModel
import org.junit.Test
import de.fosd.typechef.crefactor.util.TimeMeasurement
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.parser.c.{ConditionalNavigation, ASTNavigation}


trait BusyBoxEvaluation extends BBEval with ASTNavigation with ConditionalNavigation {

    val FORCE_VARIABILITY = true
    val MAX_DEPTH = 27
    val amountOfRefactorings = 3
    val MAX = 1

    @Test
    def evaluate() {
        val files = getBusyBoxFiles.reverse
        val refactor = files.map(file => {
            val bb_file = new File(busyBoxPath + file)
            try {
                var stats = List[Any]()
                val parseTypeCheckMs = new TimeMeasurement
                val parsed = parse(bb_file)
                val ast = parsed._1
                val fm = parsed._2
                val morpheus = new Morpheus(ast, fm)
                val parseTypeCheckTime = parseTypeCheckMs.getTime
                stats ::= parseTypeCheckTime
                runRefactor(morpheus, stats, bb_file, fm, 0, MAX)
            } catch {
                case e: Exception => {
                    println(e.getMessage)
                    println(e.getStackTrace.mkString("\n"))
                    writeExeception(e.getMessage + "\n" + e.getStackTrace.mkString("\n"), bb_file.getCanonicalPath, 0)
                    false
                }
            }
        })
        println("Refactor succ: " + refactor.contains(false))
        refactor.contains(false)
    }

    def runRefactor(morpheus: Morpheus, stats: List[Any], bb_file: File, fm: FeatureModel, run: Int, max: Int, lastResult: Boolean = true): Boolean
}


object Verification extends BBEval {

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean = {
        val workingPath = bbFile.getCanonicalPath
        val orgFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched"))
        val refFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + bbFile.getName)
        val resultDir = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/")

        val configs = resultDir.listFiles(new FilenameFilter {
            def accept(input: File, file: String): Boolean = file.endsWith(".config")
        })

        val result = configs.map(config => {
            def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
                val buildResult = buildBusyBox
                val testResult = runTest
                writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + config.getName + ext + ".build")
                if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + config.getName + ext + ".buildErr")
                writeResult(testResult, resultDir.getCanonicalPath + "/" + config.getName + ext + ".test")
                busyBoxFile.delete()
                (buildResult._1, testResult)
            }

            // clean dir first
            runScript("./buildClean.sh", busyBoxPath)

            val configBuild = new File(busyBoxPath + ".config")
            copyFile(config, configBuild)

            val orgTest = buildAndTest(bbFile, "_org")

            val buildRefFile = new File(workingPath)

            // clean dir first
            runScript("./buildClean.sh", busyBoxPath)

            // Replace original file with refactored file
            copyFile(refFile, buildRefFile)

            val refTest = buildAndTest(buildRefFile, "_ref")

            // Restore old original file again
            copyFile(orgFile, new File(workingPath))
            configBuild.delete()

            if (!orgTest._1) {
                writeError("Invalid Config.\n", resultDir.getCanonicalPath + "/" + config.getName, run)
                writeResult("Invalid Config", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                true
            } else if (refTest._1) {
                writeResult(orgTest.equals(refTest).toString, resultDir.getCanonicalPath + "/" + config.getName + ".result")
                val succ = orgTest.equals(refTest)
                if (!succ) writeError("Test failed!\n", resultDir.getCanonicalPath + "/" + config.getName, run)
                succ
            } else {
                writeError("Refactor build failed!\n", resultDir.getCanonicalPath + "/" + config.getName, run)
                writeResult("Refactor build failed!", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                false
            }

        })
        !result.contains(false)
    }

    def runTest: String = {
        val result = runScript("./runtest", busyBoxPath + "testsuite/")
        val stream = streamsToString(result)
        stream._1
    }

    def buildBusyBox: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", busyBoxPath)
        val stream = streamsToString(result)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

}
