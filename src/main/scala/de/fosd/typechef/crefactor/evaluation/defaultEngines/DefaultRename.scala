package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation.{Evaluation, StatsCan, Refactoring}
import de.fosd.typechef.crefactor.{CRefactorFrontend, Morpheus}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.crefactor.backend.engine.CRenameIdentifier
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import scala.collection.mutable
import de.fosd.typechef.error.Position
import java.io.File
import de.fosd.typechef.crefactor.backend.CLinking
import de.fosd.typechef.typesystem._
import scala.Some
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.typesystem.CUnknown
import de.fosd.typechef.typesystem.CFunction
import de.fosd.typechef.conditional.One
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.conditional.Opt


trait DefaultRename extends Refactoring with Evaluation {

    val REFACTOR_NAME = "refactoredID"

    val REFACTOR_AMOUNT = 50

    private val renameLink = Set[String]()

    private val linkedRenamedFiles = mutable.HashMap[String, Morpheus]()

    def refactor(morpheus: Morpheus): (Boolean, TranslationUnit, List[FeatureExpr], List[(String, TranslationUnit)]) = {

        var runMorpheus = morpheus
        val affectedFeatures = Set[FeatureExpr]()

        for (run <- 1 to REFACTOR_AMOUNT) {
            val refactoredRun = singleRefactor(runMorpheus, run)
            StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, refactoredRun._3)
            if (!refactoredRun._1) return (false, null, List(), List())
            refactoredRun._3.foreach(affectedFeatures + _)
            runMorpheus = new Morpheus(refactoredRun._2, morpheus.getFM, morpheus.getLinkInterface, morpheus.getFile)
            writeRunResult(run, runMorpheus, refactoredRun._4)
        }

        (true, runMorpheus.getTranslationUnit, affectedFeatures.toList, linkedRenamedFiles.toList.map(entry => (entry._1, entry._2.getTranslationUnit)))
    }

    private def singleRefactor(morpheus: Morpheus, run: Int): (Boolean, TranslationUnit, List[FeatureExpr], List[(String, TranslationUnit)]) = {
        val linkInterface = morpheus.getLinkInterface
        val name = REFACTOR_NAME + "_" + run
        logger.info("+++ Start run: " + run)

        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id): Boolean = !id.name.contains("_main") && {
                if (linkInterface != null) !(linkInterface.isBlackListed(id.name) || renameLink.contains(id.name))
                else true
            }

            // TODO Fix Bug in OpenSSL for functions without body
            def isWritable(id: Id): Boolean =
                morpheus.linkage(id).map(_.entry).forall(i =>
                    isValidId(i) && (i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile) || new File(i.getFile.get.replaceFirst("file ", "")).canWrite))

            val allIds = morpheus.getUseDeclMap.keys
            val linkedIds = if (FORCE_LINKING && linkInterface != null) allIds.par.filter(id => linkInterface.isListed(Opt(parentOpt(id, morpheus.getASTEnv).feature, id.name), morpheus.getFM)) else allIds
            val ids = if (linkedIds.isEmpty) allIds else linkedIds

            logger.info("Run " + run + ": IDs found: " + ids.size)

            val variableIds = ids.par.filter(id => isVariable(parentOpt(id, morpheus.getASTEnv)))

            logger.info("Run " + run + ": Variable IDs found: " + variableIds.size)

            def getRandomID: Id = {
                val randID = if (!variableIds.isEmpty && FORCE_VARIABILITY) variableIds.apply((math.random * variableIds.size).toInt) else ids.apply((math.random * ids.size).toInt)
                if (isWritable(randID)) randID
                else getRandomID
            }

            val id = getRandomID
            val associatedIds = morpheus.linkage(id)
            addType(associatedIds, morpheus, run)
            logger.info("Run " + run + ": Found Id: " + id)
            logger.info("Run " + run + ": Associated Ids: " + associatedIds.size)
            logger.info(morpheus.getASTEnv.containsASTElem(id))
            (id, associatedIds.length, associatedIds.map(morpheus.getASTEnv.featureExpr).distinct)
        }

        val time = new StopClock
        val toRename = getVariableIdToRename
        val determineTime = time.getTime
        logger.info("Run " + run + ": Time to determine id: " + time.getTime)
        StatsCan.addStat(morpheus.getFile, run, RandomRefactorDeterminationTime, determineTime)
        val id = toRename._1
        StatsCan.addStat(morpheus.getFile, run, RenamedId, id.name)

        val refactorChain = if (linkInterface != null) getLinkedFilesToRefactor(linkInterface, id)
        else List()

        if (refactorChain == null) return (false, null, List(), List())
        if (!refactorChain.isEmpty) renameLink + name

        val features = toRename._3
        StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, features)

        val startRenaming = new StopClock
        val refactored = CRenameIdentifier.rename(id, name, morpheus)

        StatsCan.addStat(morpheus.getFile, run, RefactorTime, startRenaming.getTime)

        refactored match {
            case Right(ast) => {
                val linkedRefactored = refactorChain.map(x => {
                    val linkedId = findIdInAST(x._2, id, x._1.getTranslationUnit)
                    val time = new StopClock
                    val ref = CRenameIdentifier.rename(linkedId.get, name, x._1)
                    StatsCan.addStat(x._1.getFile, run, RefactorTime, time.getTime)
                    ref match {
                        case Right(refAST) => (x._1.getFile, refAST)
                        case Left(s) => return (false, null, List(), List())
                    }
                })
                (true, ast, features, linkedRefactored)
            }
            case Left(s) =>
                logger.error("Refactoring faild in run " + run + " at file " + morpheus.getFile + " with " + s + ".")
                (false, null, List(), List())
        }

    }

    private def findIdInAST(position: Position, id: Id, tUnit: TranslationUnit) =
        filterASTElems[Id](tUnit).par.find(aId =>
            (position.equals(aId.getPositionFrom) || position.equals(aId.getPositionTo)) // TODO Range Check
                && aId.name.equalsIgnoreCase(id.name))


    private def getLinkedFilesToRefactor(linkInterface: CLinking, id: Id): List[(Morpheus, Position)] = {
        val linked = linkInterface.getPositions(id.name)
        val affectedFiles = linked.foldLeft(new mutable.HashMap[String, Position])((map, pos) => map += (pos.getFile -> pos))
        val refactorChain = affectedFiles.foldLeft(List[(Morpheus, Position)]())((list, entry) => {
            if (blackListFiles.exists(getFileName(entry._1).equalsIgnoreCase)) {
                logger.info("File " + getFileName(entry._1) + " is blacklisted and cannot be build.")
                return null
            }

            linkedRenamedFiles.get(entry._1) match {
                case Some(morpheus) => list :+(morpheus, entry._2)
                case _ =>
                    val linked = CRefactorFrontend.parseOrLoadTUnit(entry._1)
                    list :+(new Morpheus(linked._1, linked._2, entry._1), entry._2)
            }
        })

        refactorChain
    }

    private def addType(ids: List[Opt[Id]], morpheus: Morpheus, run: Int) = {
        val foundTypes = mutable.Set[String]()

        def addChoice(c: Choice[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True): Unit = {
            c match {
                case c@Choice(cft, o1@One(_), o2@One(_)) =>
                    addOne(o1, id)
                    addOne(o2, id, cft.not())
                case c@Choice(cft, c1@Choice(_, _, _), o2@One(_)) =>
                    addChoice(c1, id)
                    addOne(o2, id, cft.not())
                case c@Choice(cft, o1@One(_), c1@Choice(_, _, _)) =>
                    addChoice(c1, id, cft.not())
                    addOne(o1, id)
                case c@Choice(cft, c1@Choice(_, _, _), c2@Choice(_, _, _)) =>
                    addChoice(c1, id)
                    addChoice(c2, id, cft.not())
            }
        }

        def addOne(o: One[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True) = {
            if (ft.isTautology(morpheus.getFM)) {
                o match {
                    // only variables are interesting
                    case o@One((CUnknown(_), _, _)) => logger.warn("Unknown Type " + id + " " + o)
                    case o@One((CFunction(_, _), _, _)) => foundTypes + "FunctionName"
                    case o@One((CType(CFunction(_, _), _, _, _), _, _, _)) => foundTypes + "FunctionName"
                    case o@One((_, KEnumVar, _, _)) => foundTypes + "Enum"
                    case o@One((CType(_, _, _, _), _, _, _)) => foundTypes + "Variable"
                    case o => logger.warn("Unknown Type " + id + " " + o)
                }
            }
        }




        ids.map(id => {
            try {
                // only lookup variables
                morpheus.getEnv(id.entry).varEnv.lookup(id.entry.name) match {
                    case o@One(_) => addOne(o, id.entry)
                    case c@Choice(_, _, _) => addChoice(c, id.entry)
                    case x => logger.warn("Missed pattern choice? " + x)
                }
            } catch {
                case _: Throwable => foundTypes + "TypeDef"
            }
        })

        StatsCan.addStat(morpheus.getFile, run, Type, foundTypes)
    }
}
