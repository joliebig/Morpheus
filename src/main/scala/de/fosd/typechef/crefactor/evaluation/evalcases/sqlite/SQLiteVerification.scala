package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.sqlite.SQLiteEvaluation
import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.FeatureModel


object SQLiteVerification extends SQLiteEvaluation with Verification {
    def verify(evalFile: String, fm: FeatureModel, mode: String) = ???
    def runTest = ???
    def build = ???
}
