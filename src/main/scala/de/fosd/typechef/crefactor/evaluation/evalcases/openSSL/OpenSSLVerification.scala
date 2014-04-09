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
        configure()
        val defRef = buildAndTest(resultDir, -1, "_ref")
        logger.info("Can build and test " + evalFile + " in def config and ref: " + defRef)
        configureBuildAndTestFeatureCombinations(evalFile, resultDir, featureCombinations, "_ref")

        // clean up the refactor mess
        runScript(cleanScript, sourcePath)

        // run original
        //first defConfig
        configure()
        val defOrg = buildAndTest(resultDir, -1, "_org")
        logger.info("Can build and test " + evalFile + " in def config and org: " + defOrg)
        configureBuildAndTestFeatureCombinations(evalFile, resultDir, featureCombinations, "_org")

        // cleanup
        runScript(cleanScript, sourcePath)
    }

    private def configureBuildAndTestFeatureCombinations(evalFile: String, resultDir: File,
                                                         featureCombinations: List[SimpleConfiguration], mode: String) =
        featureCombinations.zipWithIndex.foreach {
            case (config, index) => {
                val conf = configure(config)
                logger.info(config.getTrueSet + " can be configured: " + conf)
                if (conf) {
                    val build = buildAndTest(resultDir, index, mode)
                    logger.info("Can build and test " + evalFile + " as " + mode + ": " + build)
                }
            }
        }

    override def configure(configuration: SimpleConfiguration): Boolean = {
        val features = configuration.getTrueSet.map(_.feature).mkString("-D", " -D", "")
        val run = runScript(confScript, sourcePath, features, runTimeout)
        evaluateScriptResult(run)._1
    }



}
