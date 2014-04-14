package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.{SingleFeatureExpr, FeatureExpr, FeatureModel}
import java.io.{Writer, File}
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
        val fw = new java.io.FileWriter(new File(resultDir.getCanonicalPath)) // TODO correct path
        featureCombinations foreach (_.getTrueSet foreach (println))
        // add def config first
        featureCombinations foreach(configure(_, fw))

        fw.flush
        fw.close
    }



}
