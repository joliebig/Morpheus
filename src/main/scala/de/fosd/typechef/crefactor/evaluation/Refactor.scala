package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{TranslationUnit, AST}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.backend.CLinking

trait Refactoring {

    def refactor(morpheus: Morpheus):
    (Boolean, AST, List[FeatureExpr], List[(String, TranslationUnit)])

}

trait Refactor {

    def rename(tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CLinking = null)

    def extract(tunit: TranslationUnit, fm: FeatureModel,
                file: String, linkInterface: CLinking = null)

    def inline(tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CLinking = null)
}


