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
        logger.info("+++ Prepared refactorings for file " + morpheus.getFile)
        logger.info("Renaming: " + renameIDs.size)
        logger.info("Extract: " + extractStmts.size)
        logger.info("Inline: " + inlineIDs.size)
        logger.info("Variability Forced: " + FORCE_VARIABILITY)
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

        def findPriorStatement(a : Product, statement : Statement) : Option[Statement] = {

            def eqStmt(stmt: Option[Statement]) =
                stmt match {
                    case Some(corStmt) =>
                        if (corStmt.equals(statement)) Some(corStmt)
                        else findPriorStatement(corStmt, statement)
                    case None => None
                }

            a match {
                case Some(i@Id(_)) =>
                    eqStmt(findPriorASTElem[Statement](i, morpheus.getASTEnv))
                case s: Statement =>
                    eqStmt(findPriorASTElem[Statement](parentAST(s, morpheus.getASTEnv), morpheus.getASTEnv))
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
                findPriorStatement(tUnitId, stmt)
            }
        })

        if (correspondingStmts.contains(None)) None
        else Some(correspondingStmts.map(_.get))
    }
}