package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.{SingleFeatureExpr, FeatureExpr, FeatureModel}
import java.io.File
import de.fosd.typechef.{KnownFeatures, ConfigFeatures, SimpleConfiguration}
import de.fosd.typechef.crefactor.evaluation.util.StopClock


object OpenSSLVerification extends OpenSSLEvaluation with Verification {

    override def singleVerify(evalFile: String, fm: FeatureModel, mode: String, affectedFeatures: List[FeatureExpr] = List()) = {
        // not supported
    }

    override def completeVerify(evalFile: String, fm: FeatureModel, affectedFeatures: List[FeatureExpr] = List()) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/")
        if (!resultDir.exists)
            resultDir.mkdirs

        val confFeatures = new ConfigFeatures(allFeatures._1)

        // get features
        val featureCombinations = getFeatureCombinations(confFeatures, affectedFeatures)

        // run refactored run first
        //first defConfig
        configOpenSSL()
        val defRef = buildAndTestOpenSSL(resultDir, -1, "_ref")
        logger.info("Can build and test " + evalFile + " in def config and ref: " + defRef)
        configureBuildAndTestFeatureCombinations(evalFile, resultDir, featureCombinations, "_ref")

        // clean up the refactor mess
        runScript(cleanScript, sourcePath)

        // run original
        //first defConfig
        configOpenSSL()
        val defOrg = buildAndTestOpenSSL(resultDir, -1, "_org")
        logger.info("Can build and test " + evalFile + " in def config and org: " + defOrg)
        configureBuildAndTestFeatureCombinations(evalFile, resultDir, featureCombinations, "_org")

        // cleanup
        runScript(cleanScript, sourcePath)
    }

    private def configureBuildAndTestFeatureCombinations(evalFile: String, resultDir: File,
                                                         featureCombinations: List[SimpleConfiguration], mode: String) =
        featureCombinations.zipWithIndex.foreach {
            case (config, index) => {
                val conf = configOpenSSL(config)
                logger.info(config.getTrueSet + " can be configured: " + conf)
                if (conf) {
                    val build = buildAndTestOpenSSL(resultDir, index, mode)
                    logger.info("Can build and test " + evalFile + " as " + mode + ": " + build)
                }
            }
        }

    private def configOpenSSL(configuration: SimpleConfiguration): Boolean = {
        val features = configuration.getTrueSet.map(_.feature).mkString("-D", " -D", "")
        val run = runScript(confScript, sourcePath, features, runTimeout)
        evaluateScriptResult(run)._1
    }

    private def configOpenSSL(): Boolean = {
        val run = runScript(confScript, sourcePath, runTimeout)
        evaluateScriptResult(run)._1
    }

    private def buildAndTestOpenSSL(resultDir: File, run: Int, mode: String): Boolean = {
        val result = runScript(buildScript, sourcePath, runTimeout)
        val buildResult = evaluateScriptResult(result)
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + run + mode + ".build")
        if (!buildResult._1)
            writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + run + mode + "buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + run + mode + "test")
        if (!testResult._1)
            writeResult(testResult._3, resultDir.getCanonicalPath + "/" + run + mode + "testError")

        logger.info("Pass build: " + buildResult._1)
        logger.info("Pass test: " + testResult._1)

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + run + mode + "result")
        testResult._1 && buildResult._1
    }

}
