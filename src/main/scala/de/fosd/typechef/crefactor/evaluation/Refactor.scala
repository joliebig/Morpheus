package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.{Statement, Id, AST, TranslationUnit}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.backend.CModuleInterface
import java.io.{ObjectOutputStream, FileOutputStream}
import java.util.zip.GZIPOutputStream

trait Refactoring {

    def refactor(morpheus: Morpheus, preparedRefactorings : PreparedRefactorings):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)])

    def getValidIdsForEvaluation(morpheus: Morpheus): List[Id]

    def getValidStatementsForEvaluation(morpheus: Morpheus): List[List[Statement]]

}

trait Refactor extends Evaluation {

    def prepareForEvaluation(tunit: TranslationUnit, fm: FeatureModel,
                             file: String, linkInterface: CModuleInterface = null) : PreparedRefactorings = {
        val morpheus = new Morpheus(tunit, fm, linkInterface, file)
        val renameIDs = renameEngine.getValidIdsForEvaluation(morpheus)
        val extractStmts = extractEngine.getValidStatementsForEvaluation(morpheus)
        val inlineIDs = inlineEngine.getValidIdsForEvaluation(morpheus)
        PreparedRefactorings(renameIDs, extractStmts, inlineIDs)
    }

    def rename(preparedRefactorings : PreparedRefactorings, tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CModuleInterface = null) =
        evaluate(preparedRefactorings, tunit, fm, file, linkInterface, renameEngine)

    def extract(preparedRefactorings : PreparedRefactorings, tunit: TranslationUnit, fm: FeatureModel,
                file: String, linkInterface: CModuleInterface = null) =
        evaluate(preparedRefactorings, tunit, fm, file, linkInterface, extractEngine)

    def inline(preparedRefactorings : PreparedRefactorings, tunit: TranslationUnit, fm: FeatureModel,
               file: String, linkInterface: CModuleInterface = null) =
        evaluate(preparedRefactorings, tunit, fm, file, linkInterface, inlineEngine)
}

case class PreparedRefactorings(renaming : List[Id], extract : List[List[Statement]], inline: List[Id]) extends Serializable {}



