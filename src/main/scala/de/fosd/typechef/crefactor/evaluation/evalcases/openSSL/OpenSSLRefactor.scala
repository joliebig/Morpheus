package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Refactor
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking

object OpenSSLRefactor extends OpenSSLEvaluation with Refactor {
    def rename(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = ???
    def extract(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = ???
    def inline(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = ???
}
