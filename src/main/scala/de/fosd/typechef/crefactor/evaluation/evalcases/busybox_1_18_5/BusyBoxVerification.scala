package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5

import java.io.{FilenameFilter, File}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.crefactor.evaluation.{StatsCan, Verification}
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.SimpleConfiguration

object BusyBoxVerification extends BusyBoxEvaluation with Verification {

    override def singleVerify(evalFile: String, fm: FeatureModel, mode: String, affectedFeatures : List[FeatureExpr] = List()): Unit = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result"))
        val configs = resultDir.listFiles(new FilenameFilter {
            def accept(input: File, file: String): Boolean = file.endsWith(".config")
        })

        StatsCan.addStat(evalFile, Variants, configs.length)

        val result = configs.map(config => {
            logger.info("Testing config: " + config.getName + " for " + evalFile + " in mode: " + mode + ".")
            def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
                val buildTestTime = System.currentTimeMillis()
                val buildResult = build
                val testResult = runTest
                logger.info("Build and test duration in ms: " + (System.currentTimeMillis() - buildTestTime))
                writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + config.getName + ext + ".build")
                if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + config.getName + ext + ".buildErr")
                writeResult(testResult, resultDir.getCanonicalPath + "/" + config.getName + ext + ".test")
                (buildResult._1, testResult)
            }

            val configBuild = new File(sourcePath + ".config")

            copyFile(config, configBuild)

            val buildTest = buildAndTest(new File(evalFile), mode)

            configBuild.delete()

            if (!buildTest._1) {
                writeError("Invalid Config.\n", resultDir.getCanonicalPath + "/" + config.getName)
                writeResult("Invalid Config", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                logger.error("Invalid config: " + config.getName + " for " + evalFile + " in mode: " + mode)
                true
            } else if (buildTest._1) {
                writeResult("true", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                logger.info("Test passed: " + config.getName + " for " + evalFile + " in mode: " + mode)
                true
            } else {
                writeError("Refactor build failed!\n", resultDir.getCanonicalPath + "/" + config.getName)
                writeResult("Refactor build failed!", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                logger.info("Test failed: " + config.getName + " for " + evalFile + " in mode: " + mode)
                false
            }
        })
        logger.info(evalFile + " passed build and testing as " + mode + ": " + result.exists(x => x))
    }

    def runTest: String = {
        val result = runScript("./runtest", sourcePath + "testsuite/")
        val stream = streamsToString(result)
        runScript("./cleanTests.sh", sourcePath)
        stream._1
    }

    override def build: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", sourcePath)
        evaluateScriptResult(result)
    }

}