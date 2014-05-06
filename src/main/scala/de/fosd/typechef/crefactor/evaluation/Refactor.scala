package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{TranslationUnit, AST}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.backend.CModuleInterface

trait Refactoring {

    def refactor(morpheus: Morpheus):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)])

    def getAvailableElementsForEvaluation[T <: AST](morpheus: Morpheus): List[T] = List()

}

trait Refactor {

    def rename(tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CModuleInterface = null)

    def extract(tunit: TranslationUnit, fm: FeatureModel,
                file: String, linkInterface: CModuleInterface = null)

    def inline(tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CModuleInterface = null)
}



