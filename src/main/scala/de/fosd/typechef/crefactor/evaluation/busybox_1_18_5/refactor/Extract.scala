package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.refactor

import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{AST, CompoundStatement}
import de.fosd.typechef.crefactor.backend.refactor.CExtractFunction
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.{PrepareASTforVerification, BusyBoxRefactor}
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.evaluation.StatsJar
import de.fosd.typechef.crefactor.evaluation.Stats._


object Extract extends BusyBoxRefactor {

    private val MAX_REC_DEPTH: Int = 1000

    private val RETRIES: Int = 3

    private val NAME = "refactored_func"

    def refactor(morpheus: Morpheus, linkInterface: CLinking): (Boolean, List[FeatureExpr]) = {
        def refactor(morpheus: Morpheus, linkInterface: CLinking, depth: Int): (Boolean, List[FeatureExpr]) = {
            val compStmts = filterAllASTElems[CompoundStatement](morpheus.getAST)

            def getRandomStatements(depth: Int = 0): List[AST] = {
                val compStmt = compStmts.apply(util.Random.nextInt(compStmts.length))
                // Ignore empty compound statements
                if (compStmt.innerStatements.length <= 0) return getRandomStatements(depth)
                val rand1 = util.Random.nextInt(compStmt.innerStatements.length)
                val rand2 = util.Random.nextInt(compStmt.innerStatements.length)
                val statements = if (rand1 < rand2) constantSlice(compStmt.innerStatements, rand1, rand2).map(_.entry) else constantSlice(compStmt.innerStatements, rand2, rand1).map(_.entry)

                if (CExtractFunction.isAvailable(morpheus, statements)) statements
                else if (depth > MAX_REC_DEPTH) List[AST]()
                else getRandomStatements(depth + 1)
            }

            def getRandomVariableStatements(depth: Int = 0): List[AST] = {
                val statements = getRandomStatements()
                if ((statements.isEmpty /*|| !statements.par.exists(isVariable(_)) */) && (depth < MAX_REC_DEPTH)) getRandomVariableStatements(depth + 1)
                else statements
            }

            val statements = getRandomVariableStatements()

            if (statements.isEmpty) {
                println("no valid statement found")
                return (false, null)
            }

            val features = filterAllOptElems(statements).map(morpheus.getASTEnv.featureExpr(_)).distinct
            val refactorTime = new TimeMeasurement
            statements.foreach(stmt => println(stmt + " " + stmt.getPositionFrom))
            val refactored = CExtractFunction.extract(morpheus, statements, NAME)
            refactored match {
                case Right(a) => {
                    write(a, morpheus.getFile)
                    PrepareASTforVerification.makeConfigs(a, morpheus.getFeatureModel, morpheus.getFile, features)
                    StatsJar.addStat(morpheus.getFile, RefactorTime, refactorTime.getTime)
                    StatsJar.addStat(morpheus.getFile, Statements, statements)
                    (true, features)
                }
                case Left(s) => {
                    // TODO Correct Error Handling
                    println("error")
                    println(s)
                    if (depth < RETRIES) refactor(morpheus, linkInterface, depth + 1)
                    else (false, null)
                }

            }
        }
        refactor(morpheus, linkInterface, 0)
    }
}
