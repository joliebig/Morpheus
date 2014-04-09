package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import java.io.File
import de.fosd.typechef.{SimpleConfiguration, ConfigFeatures}


object SQLiteVerification extends SQLiteEvaluation with Verification {

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

        // ref first
        // default (build + test)
        configure()
        val defRef = buildAndTest(resultDir, -1, "_ref")
        logger.info("Can build and test " + evalFile + " in def config and ref: " + defRef)
        // feature combos

        // clean up the refactor mess
        runScript(cleanScript, sourcePath)

        //org second
        // default (build + test)
        configure()
        val defOrg = buildAndTest(resultDir, -1, "_org")
        logger.info("Can build and test " + evalFile + " in def config and org: " + defOrg)
        // feature combos

        runScript(cleanScript, sourcePath)

    }

    override def configure(configuration: SimpleConfiguration) = false
    override def configure() = false


}
