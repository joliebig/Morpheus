package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking

trait Refactor extends Evaluation {

    def evaluate(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking)

    def refactor(morpheus: Morpheus, linkInterface: CLinking): (Boolean, List[FeatureExpr])
}
