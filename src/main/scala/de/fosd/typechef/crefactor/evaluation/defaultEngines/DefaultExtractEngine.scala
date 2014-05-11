package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation.{PreparedRefactorings, StatsCan, Evaluation, Refactoring}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.engine.CExtractFunction
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.conditional.Opt
import java.io.File
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.conditional.Opt
import scala.util.Random

trait DefaultExtractEngine extends Refactoring with Evaluation {

    val NAME = "refactored_func"

    def getValidStatementsForEvaluation(morpheus: Morpheus): List[List[Statement]] = {
        // Test all available combinations for extraction
        def getAvailableInnerStatements(opts: List[Opt[Statement]], i: Int, length: Int): List[List[Statement]] = {
            (i to length).flatMap(x => {
                val selectedElements = constantSlice(opts, i, x).map(_.entry)
                if (CExtractFunction.canRefactor(morpheus, selectedElements)) Some(selectedElements)
                else None
            }).toList
        }

        def getAvailableExtractStatements(compStmt: CompoundStatement): List[List[Statement]] = {
            val length = compStmt.innerStatements.length - 1
            if (compStmt.innerStatements.isEmpty) List()
            else (0 to length).flatMap(getAvailableInnerStatements(compStmt.innerStatements, _, length)).toList
        }

        val compStmts = filterAllASTElems[CompoundStatement](morpheus.getTranslationUnit)

        val allCombinations = compStmts.flatMap(getAvailableExtractStatements).filterNot(_.isEmpty)

        logger.info(morpheus.getFile + " Statements found to extract: " + allCombinations.size)

        val variableCombinations = allCombinations.par.filter(_.exists(isVariable)).toList

        logger.info(morpheus.getFile + " Variable statements found to extract: " + allCombinations.size)

        if (FORCE_VARIABILITY && variableCombinations.nonEmpty) Random.shuffle(variableCombinations)
        else if (FORCE_VARIABILITY) List()
        else Random.shuffle(allCombinations)
    }

    // not supported
    def getValidIdsForEvaluation(morpheus : Morpheus) : List[Id] = List()


    def refactor(morpheus: Morpheus, preparedRefactorings: PreparedRefactorings):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        val resultDir = getResultDir(morpheus.getFile)
        val path = resultDir.getCanonicalPath + File.separatorChar + getFileName(morpheus.getFile)

        if (preparedRefactorings.extract.isEmpty) {
            writeError("no valid extract statement found", path + "stmt")
            logger.warn("no valid extract statement found")
            return (false, null, List(), List())
        }

        // TODO ASK Joerg about amount of stmts
        val preparedStmts = preparedRefactorings.extract.head

        val statements = preparedRefactorings.getCorrespondingStmts(preparedStmts, morpheus) match {
            case Some(stmts) => stmts
            case _ => List()
        }

        if (statements.isEmpty) {
            writeError("no valid extract statement found", path + ".stmt")
            logger.warn("no valid extract statement found")
            return (false, null, List(), List())
        }

        val features = filterAllOptElems(statements).map(morpheus.getASTEnv.featureExpr(_)).distinct
        val refactorTime = new StopClock
        statements.foreach(stmt => println(stmt + " " + stmt.getPositionFrom))
        val refactored = CExtractFunction.extract(morpheus, statements, NAME)
        refactored match {
            case Right(a) => {
                StatsCan.addStat(morpheus.getFile, RefactorTime, refactorTime.getTime)
                StatsCan.addStat(morpheus.getFile, Statements, statements)
                (true, a, List(features), List())
            }
            case Left(s) => {
                logger.error(s)
                writeError("Refactor Error:\n" + s, path + "ref")
                (false, null, List(), List())
            }

        }
    }
}
