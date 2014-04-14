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

}
