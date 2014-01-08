package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.refactor

import java.io.File
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.crefactor.{CRefactorFrontend, Morpheus}
import de.fosd.typechef.crefactor.backend.refactor.CRenameIdentifier
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.BusyBoxEvaluation
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.evaluation.{Refactoring, StatsJar}
import de.fosd.typechef.crefactor.evaluation.Stats._
import scala.collection.mutable
import de.fosd.typechef.error.Position
import scala.Predef._
import de.fosd.typechef.parser.c.Id


object Rename extends BusyBoxEvaluation with Refactoring {

    private val REFACTOR_NAME = "refactoredID"

    def refactor(morpheus: Morpheus, linkInterface: CLinking): (Boolean, AST, List[FeatureExpr], List[(String, AST)]) = {
        def findIdInAST(position: Position, id: Id, ast: AST) = filterASTElems[Id](ast).par.find(aId => (position.equals(aId.getPositionFrom) || position.equals(aId.getPositionTo)) && aId.name.equalsIgnoreCase(id.name))

        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id): Boolean = !id.name.contains("_main") && !linkInterface.isBlackListed(id.name)

            val allIds = morpheus.getUseDeclMap.values.toArray(Array[List[Id]]()).par.foldLeft(List[Id]())((list, entry) => list ::: entry)
            val linkedIds = if (FORCE_LINKING) allIds.par.filter(id => linkInterface.isListed(id.name)) else allIds
            val ids = if (linkedIds.isEmpty) allIds else linkedIds

            val writeAbleIds = ids.filter(id =>
                CRenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap).par.forall(i =>
                    isValidId(i) && i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile) && new File(i.getFile.get.replaceFirst("file ", "")).canWrite))

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
        StatsJar.addStat(morpheus.getFile, RenamedId, id.name)

        val linked = linkInterface.getPositions(id.name)
        val affectedFiles = linked.foldLeft(new mutable.HashMap[String, Position])((map, pos) => map += (pos.getFile -> pos))
        val refactorChain = affectedFiles.foldLeft(List[(Morpheus, Position)]())((list, entry) => {
            if (blackListFiles.exists(getFileName(entry._1).equalsIgnoreCase)) {
                println("+++ File is blacklisted and cannot be build +++")
                return (false, null, List(), List())
            }
            val ast = CRefactorFrontend.parse(entry._1)
            list :+(new Morpheus(ast._1, ast._2, entry._1), entry._2)
        })

        val features = toRename._3
        StatsJar.addStat(morpheus.getFile, AffectedFeatures, features)

        val startRenaming = new TimeMeasurement
        val refactored = CRenameIdentifier.rename(id, REFACTOR_NAME, morpheus)

        StatsJar.addStat(morpheus.getFile, RefactorTime, startRenaming.getTime)

        refactored match {
            case Right(ast) => {
                val linkedRefactored = refactorChain.map(x => {
                    val linkedId = findIdInAST(x._2, id, x._1.getAST)
                    val time = new TimeMeasurement
                    val ref = CRenameIdentifier.rename(linkedId.get, REFACTOR_NAME, x._1)
                    StatsJar.addStat(x._1.getFile, RefactorTime, time.getTime)
                    ref match {
                        case Right(refAST) => (x._1.getFile, refAST)
                        case Left(s) => return (false, null, List(), List())
                    }
                })
                (true, ast, features, linkedRefactored)
            }
            case Left(s) => (false, null, List(), List())
        }
    }
}
