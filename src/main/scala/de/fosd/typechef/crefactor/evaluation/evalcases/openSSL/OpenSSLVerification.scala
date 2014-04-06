package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import java.io.File


object OpenSSLVerification extends OpenSSLEvaluation with Verification {

    override def verify(evalFile: String, fm: FeatureModel, mode: String, affectedFeatures: List[FeatureExpr] = List()) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/" + mode + "/")
        if (!resultDir.exists) resultDir.mkdirs

        // Default Config
        buildAndTestOpenSSL(resultDir, "", -1, mode)

        // Affected Configs
        // TODO Sampling
        affectedFeatures.zipWithIndex.foreach {
            case (singleFexpr, i) => buildAndTestOpenSSL(resultDir, singleFexpr.collectDistinctFeatures.mkString(" "), i, mode)
        }

    }

    private def buildAndTestOpenSSL(resultDir: File, features: String, run: Int, mode: String) : Boolean = {
        val result = runScript(buildScript, sourcePath, features, runTimeout)
        val buildResult = evaluateScriptResult(result)
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + run + mode + ".build")
        if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + run + mode + "buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + run + mode + "test")
        if (!testResult._1) writeResult(testResult._3, resultDir.getCanonicalPath + "/" + run + mode + "testError")

        logger.info("Pass build: "+ buildResult._1)
        logger.info("Pass test: " + testResult._1)

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + run + mode + "result")
        testResult._1 && buildResult._1
    }

}
