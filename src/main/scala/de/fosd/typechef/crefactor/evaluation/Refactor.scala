package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.setup.CLinking

trait Refactoring {

    def refactor(morpheus: Morpheus, linkInterface: CLinking = null): (Boolean, AST, List[FeatureExpr], List[(String, AST)])

}

trait Refactor {

    def rename(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking = null)

    def extract(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking = null)

    def inline(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking = null)
}


