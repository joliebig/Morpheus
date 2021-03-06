package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation.{PreparedRefactorings, StatsCan, Evaluation, Refactoring}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.engine.CInlineFunction
import scala.util.Random
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._

trait DefaultInlineEngine extends Refactoring with Evaluation {

    // not supported
    override def getValidStatementsForEvaluation(morpheus: Morpheus): List[List[Statement]] = List()

    override def getValidIdsForEvaluation(morpheus : Morpheus) : List[Id] = {
        val psExpr = filterAllASTElems[PostfixExpr](morpheus.getTranslationUnit)
        val funcCalls = psExpr.filter(isFunctionCall)
        val availableFuncCalls = funcCalls.flatMap(p => {
            p.p match {
                case i: Id =>
                    if (hasSameFileName(i, morpheus) && CInlineFunction.canInline(morpheus, i)) Some(i)
                    else None
                case _ => None
            }
        })

        logger.info(morpheus.getFile +  " Function calls found to inline: " + availableFuncCalls.size)

        // Prefer var func calls
        val variableFuncCalls = availableFuncCalls.filter(call => {
            val callsDeclDef = CInlineFunction.getCallDeclDefCallExprs(call, morpheus)
            val varCalls = callsDeclDef._1.exists(x => isVariable(x._2, morpheus.getASTEnv))
            val varDecls = callsDeclDef._2.exists(isVariable(_, morpheus.getASTEnv))
            val varDefs = callsDeclDef._3.exists(isVariable(_, morpheus.getASTEnv))
            val varExpr = callsDeclDef._4.exists(x => isVariable(x._2 , morpheus.getASTEnv))

            (varCalls || varDecls || varDefs || varExpr)
        })

        logger.info(morpheus.getFile +  " Variable function calls found to inline: " + variableFuncCalls.size)

        if (FORCE_VARIABILITY && variableFuncCalls.nonEmpty) Random.shuffle(variableFuncCalls)
        else if (FORCE_VARIABILITY) List()
        else Random.shuffle(variableFuncCalls) ::: Random.shuffle(availableFuncCalls.filterNot(aId => variableFuncCalls.par.exists(_.eq(aId))))
    }

    def refactor(morpheus: Morpheus, preparedRefactorings: PreparedRefactorings):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        if (preparedRefactorings.inline.isEmpty)  {
            logger.info("No refactoring prepared.")
            return (false, null, List(), List())
        }


        val preparedCallId = preparedRefactorings.inline.head

        val callIdToInline =
            preparedRefactorings.getCorrespondingId(preparedCallId, morpheus) match {
                case Some(id) => id
                case _ => null
            }

        if (callIdToInline == null)
            return (false, null, List(), List())

        logger.info("Trying to inline fcall: " + callIdToInline)

        val refTime = new StopClock
        val refAST = CInlineFunction.inline(morpheus, callIdToInline, true)

        refAST match {
            case Left(errmsg) =>
                logger.error("Inlining failed! " + errmsg)
                (false, null, List(), List())
            case Right(tunit) =>
                val timePassed = refTime.getTime
                logger.info("Refactoring took: " + timePassed + "ms")
                StatsCan.addStat(morpheus.getFile, RefactorTime, timePassed)
                val callDeclDef = CInlineFunction.getCallDeclDefCallExprs(callIdToInline, morpheus)

                val callFeatures = callDeclDef._1.map(x => x._2.feature)
                val declFeatures = callDeclDef._2.flatMap(filterAllFeatureExpr(_))
                val defFeatures = callDeclDef._3.flatMap(filterAllFeatureExpr(_))
                val exprFeatures = callDeclDef._4.map(x => x._2.feature)

                val features = (callFeatures ::: declFeatures ::: defFeatures ::: exprFeatures).distinct

                StatsCan.addStat(morpheus.getFile, Amount, callDeclDef._1.size)
                StatsCan.addStat(morpheus.getFile, InlinedFunction, callIdToInline)

                logger.info("Affected features: " + features)

                (true, tunit, List(features), List())
        }
    }

    private def isFunctionCall(p: PostfixExpr): Boolean =
        p match {
            case PostfixExpr(Id(_), FunctionCall(_)) => true
            case _ => false
        }

}
