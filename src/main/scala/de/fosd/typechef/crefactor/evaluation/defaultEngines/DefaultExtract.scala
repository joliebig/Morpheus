package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation.{StatsCan, Evaluation, Refactoring}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.parser.c.{TranslationUnit, Statement, AST, CompoundStatement}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.engine.CExtractFunction
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.conditional.Opt
import java.io.File

trait DefaultExtract extends Refactoring with Evaluation {

    val MAX_REC_DEPTH: Int = 100

    val RETRIES: Int = 10

    val NAME = "refactored_func"


    def refactor(morpheus: Morpheus): (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        val resultDir = getResultDir(morpheus.getFile)
        val path = resultDir.getCanonicalPath + File.separatorChar + getFileName(morpheus.getFile)

        def refactor(morpheus: Morpheus, depth: Int): (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
            val compStmts = filterAllASTElems[CompoundStatement](morpheus.getTranslationUnit)

            // Real random approach
            def getRandomStatements(depth: Int = 0): List[AST] = {
                val compStmt = compStmts.apply(util.Random.nextInt(compStmts.length))
                // Ignore empty compound statements
                if (compStmt.innerStatements.length <= 0) return getRandomStatements(depth)
                val rand1 = util.Random.nextInt(compStmt.innerStatements.length)
                val rand2 = util.Random.nextInt(compStmt.innerStatements.length)
                val statements = if (rand1 < rand2)
                                     constantSlice(compStmt.innerStatements, rand1, rand2).map(_.entry)
                                 else
                                     constantSlice(compStmt.innerStatements, rand2, rand1).map(_.entry)

                if (CExtractFunction.isAvailable(morpheus, statements)) statements
                else if (depth > MAX_REC_DEPTH) List[AST]()
                else getRandomStatements(depth + 1)
            }

            def getRandomVariableStatements(depth: Int = 0): List[AST] = {
                val statements = getRandomStatements()
                if ((statements.isEmpty || !statements.par.exists(isVariable(_))) && (depth < RETRIES))
                    getRandomVariableStatements(depth + 1)
                else
                    statements
            }
            // End real random approach

            // Test all available combinations for extraction
            def getAvailableInnerStatements(opts: List[Opt[Statement]], i: Int, length: Int): List[List[AST]] = {
                (i to length).foldLeft(List[List[AST]]())((l, x) => {
                    val selectedElements = constantSlice(opts, i, x).map(_.entry)
                    if (CExtractFunction.isAvailable(morpheus, selectedElements)) selectedElements :: l
                    else l
                })
            }

            def getAvailableExtractStatements(compStmt: CompoundStatement): List[List[AST]] = {
                val length = compStmt.innerStatements.length - 1
                if (compStmt.innerStatements.isEmpty) List()
                else (0 to length).foldLeft(List[List[AST]]())((l, i) => l ::: getAvailableInnerStatements(compStmt.innerStatements, i, length))
            }

            def getExtractStatements: List[AST] = {
                logger.info("Start brute force.")
                val startTime = new StopClock
                val availableStmtsToExtract = compStmts.flatMap(compSmt => getAvailableExtractStatements(compSmt)).filterNot(x => x.isEmpty)
                logger.info("Time to determine statements: " + startTime.getTime)
                // Pick a random available element from the resulting array
                if (!availableStmtsToExtract.isEmpty) availableStmtsToExtract.apply(util.Random.nextInt(availableStmtsToExtract.length))
                else List()
            }
            // End all combinations

            val statements = {
                val varStats = getRandomVariableStatements()
                if (!varStats.isEmpty) varStats
                else getExtractStatements
            }

            if (statements.isEmpty) {
                writeError("no valid extract statement found", path + "stmt")
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

        refactor(morpheus, 0)
    }

}
