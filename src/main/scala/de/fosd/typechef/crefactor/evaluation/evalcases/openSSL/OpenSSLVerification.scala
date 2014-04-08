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
        if (!resultDir.exists) resultDir.mkdirs

        val confFeatures = new ConfigFeatures(allFeatures._1)

        // get features
        val featureCombinations = getFeatureCombinations(confFeatures, affectedFeatures)

        // run refacotred run first

        // clean up the refactor mess

        // run original
        // make config
        // make depend
        // make test

        // cleanup and log result

    }

    private def buildAndTestOpenSSL(resultDir: File, features: String, run: Int, mode: String): Boolean = {
        val result = runScript(buildScript, sourcePath, features, runTimeout)
        val buildResult = evaluateScriptResult(result)
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + run + mode + ".build")
        if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + run + mode + "buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + run + mode + "test")
        if (!testResult._1) writeResult(testResult._3, resultDir.getCanonicalPath + "/" + run + mode + "testError")

        logger.info("Pass build: " + buildResult._1)
        logger.info("Pass test: " + testResult._1)

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + run + mode + "result")
        testResult._1 && buildResult._1
    }

}
