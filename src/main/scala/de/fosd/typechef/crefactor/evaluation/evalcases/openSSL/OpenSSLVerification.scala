package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import java.io.File


object OpenSSLVerification extends OpenSSLEvaluation with Verification {

    override def verify(evalFile: String, fm: FeatureModel, mode: String, affectedFeatures : List[FeatureExpr] = List()) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/" + mode + "/")
        if (!resultDir.exists) resultDir.mkdirs

        val buildResult = build
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + "build")
        if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + "buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + "test")
        if (!testResult._1) writeResult(testResult._3, resultDir.getCanonicalPath + "/" + "testError")

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + "result")
    }

}
