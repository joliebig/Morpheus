package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.{Logging, Morpheus}
import de.fosd.typechef.crefactor.backend.CModuleInterface


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

case class PreparedRefactorings(renaming : List[Id], extract : List[List[Statement]], inline: List[Id])
    extends Serializable with ASTNavigation with ConditionalNavigation with Logging {

    def getCorrespondingId(id: Id, morpheus : Morpheus) : Option[Id] =
        filterASTElems[Id](morpheus.getTranslationUnit, morpheus.getASTEnv).par.find(tunitId =>
            (tunitId.name == id.name)
                && tunitId.getPositionFrom.equals(id.getPositionFrom) && tunitId.getPositionTo.equals(id.getPositionTo))

    /**
     * Statements have no position range. We filter for an id part of the selection and "bubble up" to find the
     * corresponding statement in the current tunit.
     */
    def getCorrespondingStmts(stmts : List[Statement], morpheus : Morpheus) : Option[List[Statement]] = {

        def findPrirorStatement(a : Product, statement : Statement) : Option[Statement] = {
            findPriorASTElem[Statement](a, morpheus.getASTEnv) match {
                case Some(corStmt) =>
                    if (corStmt.equals(statement)) Some(corStmt)
                    else findPrirorStatement(corStmt, statement)
                case None => None
            }
        }

        val correspondingStmts = stmts.map(stmt => {
            val stmtIds = filterASTElems[Id](stmt)
            if (stmtIds.isEmpty) {
                logger.error("Could not determine corresponding tunit statement")
                None
            } else {
                val tUnitId = getCorrespondingId(stmtIds.head, morpheus)
                findPrirorStatement(tUnitId, stmt)
            }
        })

        if (correspondingStmts.exists(_ == None)) None
        else Some(correspondingStmts.map(_.get))
    }
}



