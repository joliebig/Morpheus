package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.{SimpleConfiguration, ConfigFeatures}


object SQLiteVerification extends SQLiteEvaluation with Verification {

    override def singleVerify(evalFile: String, fm: FeatureModel, mode: String, affectedFeatures: List[FeatureExpr] = List()) = {
        // not supported
    }

    override def configure(configuration: SimpleConfiguration) = {
        // TODO Enable them in config script.
        val features = ""
        val run = runScript(confScript, sourcePath, features, runTimeout)
        evaluateScriptResult(run)._1
    }


}
