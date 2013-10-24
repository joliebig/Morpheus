package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.FeatureModel

trait Verification {

    def verify(evalFile: String, fm: FeatureModel, mode: String)

    def runTest: String

    def build: (Boolean, String, String)

}
