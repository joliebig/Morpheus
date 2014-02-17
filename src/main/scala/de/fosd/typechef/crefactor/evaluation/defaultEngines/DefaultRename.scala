package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation.{Evaluation, StatsJar, Refactoring}
import de.fosd.typechef.crefactor.{CRefactorFrontend, Morpheus}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.engine.CRenameIdentifier
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.parser.c.Id
import scala.collection.mutable
import de.fosd.typechef.error.Position
import java.io.File
import de.fosd.typechef.crefactor.backend.CLinking


trait DefaultRename extends Refactoring with Evaluation {

    val REFACTOR_NAME = "refactoredID"

    def refactor(morpheus: Morpheus): (Boolean, AST, List[FeatureExpr], List[(String, AST)]) = {
        def findIdInAST(position: Position, id: Id, ast: AST) = filterASTElems[Id](ast).par.find(aId => (position.equals(aId.getPositionFrom) || position.equals(aId.getPositionTo)) && aId.name.equalsIgnoreCase(id.name))
        val linkInterface = morpheus.getLinkInterface

        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id): Boolean = !id.name.contains("_main") && {
                if (linkInterface != null) !linkInterface.isBlackListed(id.name)
                else true
            }

            // TODO Fix Bug in OpenSSL for functions without body
            def isWritable(id: Id): Boolean = morpheus.linkage(id).forall(i =>
                isValidId(i) && (i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile) || new File(i.getFile.get.replaceFirst("file ", "")).canWrite))

            val allIds = morpheus.getUseDeclMap.keys
            val linkedIds = if (FORCE_LINKING && linkInterface != null) allIds.par.filter(id => linkInterface.isListed(id.name)) else allIds
            val ids = if (linkedIds.isEmpty) allIds else linkedIds

            logger.info("IDs found: " + ids.size)

            /**
            val writeAbleIds = ids.filter(id =>
                morpheus.getAllConnectedIdentifier(id).forall(i =>
                    isValidId(i) && (i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile) || new File(i.getFile.get.replaceFirst("file ", "")).canWrite)))

            println("+++ Writeable IDs found: " + writeAbleIds.size) */

            val variableIds = ids.par.filter(id => isVariable(parentOpt(id, morpheus.getASTEnv)))

            logger.info("+++ Variable IDs found: " + variableIds.size)

            def getRandomID: Id = {
                val randID = if (!variableIds.isEmpty && FORCE_VARIABILITY) variableIds.apply((math.random * variableIds.size).toInt) else ids.apply((math.random * ids.size).toInt)
                if (isWritable(randID)) randID
                else getRandomID
            }

            val id = getRandomID
            val associatedIds = morpheus.linkage(id)
            println("+++ Found Id: " + id)
            println("+++ Associated Ids: " + associatedIds.size)
            (id, associatedIds.length, associatedIds.map(morpheus.getASTEnv.featureExpr).distinct)
        }

        val time = new StopClock
        val toRename = getVariableIdToRename
        val determineTime = time.getTime
        logger.info("Time to determine id: " + time.getTime)
        StatsJar.addStat(morpheus.getFile, RandomRefactorDeterminationTime, determineTime)
        val id = toRename._1
        StatsJar.addStat(morpheus.getFile, RenamedId, id.name)

        val refactorChain = if (linkInterface != null) getLinkedFilesToRefactor(linkInterface, id)
        else List()


        val features = toRename._3
        StatsJar.addStat(morpheus.getFile, AffectedFeatures, features)

        val startRenaming = new StopClock
        val refactored = CRenameIdentifier.rename(id, REFACTOR_NAME, morpheus)

        StatsJar.addStat(morpheus.getFile, RefactorTime, startRenaming.getTime)

        refactored match {
            case Right(ast) => {
                val linkedRefactored = refactorChain.map(x => {
                    val linkedId = findIdInAST(x._2, id, x._1.getTranslationUnit)
                    val time = new StopClock
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


    private def getLinkedFilesToRefactor(linkInterface: CLinking, id: Id):
    List[(Morpheus, Position)] = {
        val linked = linkInterface.getPositions(id.name)
        val affectedFiles = linked.foldLeft(new mutable.HashMap[String, Position])((map, pos) => map += (pos.getFile -> pos))
        val refactorChain = affectedFiles.foldLeft(List[(Morpheus, Position)]())((list, entry) => {
            if (blackListFiles.exists(getFileName(entry._1).equalsIgnoreCase)) {
                logger.info("File is blacklisted and cannot be build +++")
                return null
            }
            val ast = CRefactorFrontend.parseOrLoadAST(entry._1)
            list :+(new Morpheus(ast._1, ast._2, entry._1), entry._2)
        })
        refactorChain
    }
}
