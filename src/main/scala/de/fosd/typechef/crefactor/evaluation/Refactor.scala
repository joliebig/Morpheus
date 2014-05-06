package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{Statement, Id, AST, TranslationUnit}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.backend.CModuleInterface

trait Refactoring {

    def refactor(morpheus: Morpheus):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)])

    def getValidIdsForEvaluation(morpheus: Morpheus): List[Id]

    def getValidStatementsForEvaluation(morpheus: Morpheus): List[Statement]

}

trait Refactor {

    def rename(tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CModuleInterface = null)

    def extract(tunit: TranslationUnit, fm: FeatureModel,
                file: String, linkInterface: CModuleInterface = null)

    def inline(tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CModuleInterface = null)
}



