package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.refactor

import java.io.File
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel, FeatureExpr}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.backend.refactor.CRenameIdentifier
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.Declaration
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.BusyBoxRefactor
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.evaluation.StatsJar
import de.fosd.typechef.crefactor.evaluation.Stats._


object Rename extends BusyBoxRefactor {

    private val REFACTOR_NAME = "refactoredID"

    def runRefactor(morpheus: Morpheus, stats: List[Any], bb_file: File, fm: FeatureModel, run: Int, max: Int, lastResult: Boolean = true): Boolean = {
        if (run >= max) return lastResult
        val result = applyRefactor(morpheus, stats)
        if (result._1 == null) println("AST IS NULL!")
        val verify: Boolean = evalRefactoredAST(result, bb_file, run, morpheus, fm)
        verify && runRefactor(morpheus, stats, bb_file, fm, run + 1, MAX)
    }


    private def applyRefactor(morpheus: Morpheus, stat: List[Any]): (AST, Boolean, List[FeatureExpr], List[Any]) = {
        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id, morpheus: Morpheus): Boolean = {
                val isFunc = findPriorASTElem[FunctionDef](id, morpheus.getASTEnv) match {
                    case None => false
                    case entry => entry.get.declarator.getId.eq(id)
                }
                val isExternalFunc = findPriorASTElem[Declaration](id, morpheus.getASTEnv) match {
                    case None => false
                    case entry => entry.get.declSpecs.exists(spec => spec.entry match {
                        case ExternSpecifier() => true
                        case _ => false
                    })
                }
                !(id.name.equals("main") || isFunc || isExternalFunc)
            }

            val ids = morpheus.getUseDeclMap.values().toArray(Array[List[Id]]()).par.foldLeft(List[Id]())((list, entry) => list ::: entry)

            val writeAbleIds = ids.filter(id =>
                CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap).par.forall(i =>
                    new File(i.getFile.get.replaceFirst("file ", "")).canWrite && isValidId(i, morpheus) && i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile)))

            val variableIds = writeAbleIds.par.filter(id => {
                val associatedIds = CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
                val features = associatedIds.map(x => morpheus.getASTEnv.featureExpr(x))
                !(features.distinct.length == 1 && features.distinct.contains(FeatureExprFactory.True))
            })

            val id = if (!variableIds.isEmpty) variableIds.apply((math.random * variableIds.size).toInt) else writeAbleIds.apply((math.random * writeAbleIds.size).toInt)
            val associatedIds = CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
            (id, associatedIds.length, associatedIds.map(morpheus.getASTEnv.featureExpr(_)).distinct)
        }

        val toRename = getVariableIdToRename
        val id = toRename._1
        val features = toRename._3

        val startRenaming = new TimeMeasurement
        val refactored = CRenameIdentifier.rename(id, REFACTOR_NAME, morpheus)
        val renamingTime = startRenaming.getTime
        var stats = stat.::(renamingTime)
        stats = stats.::(id)
        stats = stats.::(toRename._2)
        stats = stats.::(features)

        // TODO ERROR HANDLING
        refactored match {
            case Right(ast) => {
                val morpheus_ref = new Morpheus(ast, morpheus.getFeatureModel)
                val originAmount = analsyeDeclUse(morpheus.getDeclUseMap).sorted
                val newAmount = analsyeDeclUse(morpheus_ref.getDeclUseMap).sorted
                (ast, originAmount == newAmount, features, stats)
            }
            case Left(s) => {
                (null, false, features, stats)
            }
        }

    }
    def refactor(morpheus: Morpheus, linkInterface: CLinking): Boolean = {
        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id): Boolean = !id.name.contains("_main") && !linkInterface.isBlackListed(id.name)

            val ids = morpheus.getUseDeclMap.values().toArray(Array[List[Id]]()).par.foldLeft(List[Id]())((list, entry) => list ::: entry)

            val writeAbleIds = ids.filter(id =>
                CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap).par.forall(i =>
                    new File(i.getFile.get.replaceFirst("file ", "")).canWrite && isValidId(i)))

            val variableIds = writeAbleIds.par.filter(id => {
                val associatedIds = CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
                val features = associatedIds.map(morpheus.getASTEnv.featureExpr)
                !(features.distinct.length == 1 && features.distinct.contains(FeatureExprFactory.True))
            })

            val id = if (!variableIds.isEmpty && FORCE_VARIABILITY) variableIds.apply((math.random * variableIds.size).toInt) else writeAbleIds.apply((math.random * writeAbleIds.size).toInt)
            val associatedIds = CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
            (id, associatedIds.length, associatedIds.map(morpheus.getASTEnv.featureExpr).distinct)
        }

        val time = new TimeMeasurement
        val toRename = getVariableIdToRename
        StatsJar.addStat(morpheus.getFile, RandomRefactorDeterminationTime, time.getTime)
        val id = toRename._1

        System.out.println(linkInterface)
        System.out.println(id)
        System.out.println(linkInterface.isListed(id.name))
        System.out.println(linkInterface.getPositions(id.name))


        val features = toRename._3



        false
    }
}
