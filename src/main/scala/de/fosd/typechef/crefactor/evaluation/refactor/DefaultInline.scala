package de.fosd.typechef.crefactor.evaluation.refactor

import de.fosd.typechef.crefactor.evaluation.{StatsJar, Evaluation, Refactoring}
import de.fosd.typechef.parser.c.{AST, FunctionCall, Id, PostfixExpr}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.refactor.CInlineFunction
import scala.util.Random
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.linking.CLinking

trait DefaultInline extends Refactoring with Evaluation {

    private def isFunctionCall(p: PostfixExpr): Boolean = {
        p match {
            case PostfixExpr(Id(_), FunctionCall(_)) => true
            case _ => false
        }
    }

    def refactor(morpheus: Morpheus, linkInterface: CLinking): (Boolean, AST, List[FeatureExpr], List[(String, AST)]) = {
        val psExpr = filterAllASTElems[PostfixExpr](morpheus.getAST)
        val funcCalls = psExpr.par.filter(isFunctionCall)
        val availableFuncCalls = funcCalls.par.filter(p => {
            p.p match {
                case i: Id => CInlineFunction.isAvailable(morpheus, i)
                case _ => false
            }
        }).toList

        println("+++ Function calls found to inline: " + availableFuncCalls.size)

        if (availableFuncCalls.isEmpty) return (false, null, List(), List())

        val callIdToInline = availableFuncCalls(Random.nextInt(availableFuncCalls.size)).p.asInstanceOf[Id]

        println("+++ Trying to inline call: " + callIdToInline)

        try {
            val refTime = new TimeMeasurement
            val refAST = CInlineFunction.inline(morpheus, callIdToInline, true, true)
            StatsJar.addStat(morpheus.getFile, RefactorTime, refTime.getTime)
            val callDeclDef = CInlineFunction.divideCallDeclDef(callIdToInline, morpheus)

            val callFeatures = callDeclDef._1.map(_.feature)
            val declFeatures = callDeclDef._2.flatMap(filterAllFeatureExpr(_))
            val defFeatures = callDeclDef._3.flatMap(filterAllFeatureExpr(_))

            val features = (callFeatures ::: declFeatures ::: defFeatures).distinct

            StatsJar.addStat(morpheus.getFile, Amount, callDeclDef._1.size)
            StatsJar.addStat(morpheus.getFile, InlinedFunction, callIdToInline)

            println("+++ Affected features: " + features)

            (true, refAST, features, List())

        } catch {
            case e: Exception => {
                println("+++ Inlining failed!")
                e.printStackTrace
                return (false, null, List(), List())
            }
        }
    }

}
