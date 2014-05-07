package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation.{StatsCan, Evaluation, Refactoring}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.engine.CInlineFunction
import scala.util.Random
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._

trait DefaultInline extends Refactoring with Evaluation {

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
        val variableFuncCalls = availableFuncCalls.flatMap(call => {
            val callsDeclDef = CInlineFunction.divideCallDeclDef(call, morpheus)
            val varCalls = callsDeclDef._1.exists(isVariable)
            val varDecls = callsDeclDef._2.exists(isVariable)
            val varDefs = callsDeclDef._3.exists(isVariable)
            val varExpr = callsDeclDef._4.exists(isVariable)

            if (varCalls || varDecls || varDefs || varExpr) Some(call)
            else None
        })

        logger.info(morpheus.getFile +  " Variable function calls found to inline: " + variableFuncCalls.size)

        if (FORCE_VARIABILITY && variableFuncCalls.nonEmpty) Random.shuffle(variableFuncCalls)
        else if (FORCE_VARIABILITY) List()
        else Random.shuffle(availableFuncCalls)
    }

    def refactor(morpheus: Morpheus): (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
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

        logger.info("Function calls found to inline: " + availableFuncCalls.size)

        if (availableFuncCalls.isEmpty)
            return (false, null, List(), List())

        // Prefer var func calls
        val variableFuncCalls = availableFuncCalls.flatMap(call => {
            val callsDeclDef = CInlineFunction.divideCallDeclDef(call, morpheus)
            val varCalls = callsDeclDef._1.exists(isVariable)
            val varDecls = callsDeclDef._2.exists(isVariable)
            val varDefs = callsDeclDef._3.exists(isVariable)
            val varExpr = callsDeclDef._4.exists(isVariable)

            if (varCalls || varDecls || varDefs || varExpr) Some(call)
            else None
        })

        logger.info("Variable function calls found to inline: " + variableFuncCalls.size)

        val callIdToInline =
            if (variableFuncCalls.isEmpty) availableFuncCalls(Random.nextInt(availableFuncCalls.size))
            else variableFuncCalls(Random.nextInt(variableFuncCalls.size))

        logger.info("Trying to inline fcall: " + callIdToInline)

        try {
            val refTime = new StopClock
            val refAST = CInlineFunction.inline(morpheus, callIdToInline, true)

            refAST match {
                case Left(errmsg) => {
                    logger.error("Inlining failed! " + errmsg)
                    (false, null, List(), List())
                }
                case Right(tunit) => {
                    StatsCan.addStat(morpheus.getFile, RefactorTime, refTime.getTime)
                    val callDeclDef = CInlineFunction.divideCallDeclDef(callIdToInline, morpheus)

                    val callFeatures = callDeclDef._1.map(_.feature)
                    val declFeatures = callDeclDef._2.flatMap(filterAllFeatureExpr(_))
                    val defFeatures = callDeclDef._3.flatMap(filterAllFeatureExpr(_))

                    val features = (callFeatures ::: declFeatures ::: defFeatures).distinct

                    StatsCan.addStat(morpheus.getFile, Amount, callDeclDef._1.size)
                    StatsCan.addStat(morpheus.getFile, InlinedFunction, callIdToInline)

                    logger.info("Affected features: " + features)

                    (true, tunit, List(features), List())
                }
            }
        } catch {
            case e: Exception => {
                logger.error("Inlining failed!")
                e.printStackTrace()
                return (false, null, List(), List())
            }
        }
    }

    private def isFunctionCall(p: PostfixExpr): Boolean = {
        p match {
            case PostfixExpr(Id(_), FunctionCall(_)) => true
            case _ => false
        }
    }

}
