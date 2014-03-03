package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation._
import de.fosd.typechef.crefactor.{CRefactorFrontend, Morpheus}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.crefactor.backend.engine.CRenameIdentifier
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import scala.collection.mutable
import de.fosd.typechef.error.Position
import java.io.File
import de.fosd.typechef.crefactor.backend.CModuleInterface
import de.fosd.typechef.typesystem._
import scala.Some
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.typesystem.CUnknown
import de.fosd.typechef.typesystem.CFunction
import de.fosd.typechef.conditional.One
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.conditional.Opt
import java.util


trait DefaultRename extends Refactoring with Evaluation {

    val REFACTOR_NAME = "refactoredID"

    val REFACTOR_AMOUNT = 50

    private val renameLink = new util.HashSet[String]()

    private val linkedRenamedFiles = mutable.HashMap[String, Morpheus]()

    // refactor attempts to make REFACTOR_AMOUNT renamings in a file
    def refactor(morpheus: Morpheus): (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {

        var runMorpheus = morpheus
        var affectedFeatures = List[List[FeatureExpr]]()

        for (run <- 1 to REFACTOR_AMOUNT) {
            val refactoredRun = singleRefactor(runMorpheus, run)
            StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, refactoredRun._3)

            if (!refactoredRun._1)
                return (run != 1, runMorpheus.getTranslationUnit, affectedFeatures.distinct,
                    linkedRenamedFiles.toList.map(entry => (entry._1, entry._2.getTranslationUnit)))

            affectedFeatures = refactoredRun._3 :: affectedFeatures
            runMorpheus = new Morpheus(refactoredRun._2, morpheus.getFM, morpheus.getModuleInterface, morpheus.getFile)

            refactoredRun._4.foreach(
                entry => linkedRenamedFiles.put(entry._1, new Morpheus(entry._2, runMorpheus.getFM, entry._1)))

            writeRunResult(run, runMorpheus, refactoredRun._4)
            logger.info("Run " + run + " affected features: " + refactoredRun._3)
        }

        (true, runMorpheus.getTranslationUnit, affectedFeatures.distinct,
            linkedRenamedFiles.toList.map(entry => (entry._1, entry._2.getTranslationUnit)))
    }

    // this function applies a single renaming, after checking different predicates
    // such as isValidId, ...
    private def singleRefactor(morpheus: Morpheus, run: Int): (Boolean, TranslationUnit, List[FeatureExpr], List[(String, TranslationUnit)]) = {
        val moduleInterface = morpheus.getModuleInterface
        val name = REFACTOR_NAME + "_" + run
        logger.info("+++ Start run: " + run)

        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id): Boolean = !id.name.contains("_main") && !isSystemLinkedName(id.name) && {
                if (moduleInterface != null) !(moduleInterface.isBlackListed(id.name) || renameLink.contains(id.name))
                else true
            }

            // TODO Fix Bug in OpenSSL for functions without body
            def isWritable(id: Id): Boolean =
                morpheus.getReferences(id).map(_.entry).forall(i =>
                    isValidId(i) && (i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile) || new File(i.getFile.get.replaceFirst("file ", "")).canWrite))

            val allIds = morpheus.getAllUses
            val linkedIds = if (FORCE_LINKING && moduleInterface != null)
                allIds.par.filter(id => moduleInterface.isListed(Opt(parentOpt(id, morpheus.getASTEnv).feature, id.name), morpheus.getFM))
            else allIds
            val ids = if (linkedIds.isEmpty) allIds else linkedIds

            logger.info("Run " + run + ": IDs found: " + ids.size)

            val nonRefactoredIds = ids.par.filterNot(id => id.name.startsWith(REFACTOR_NAME))
            val variableIds = nonRefactoredIds.par.filter(id => isVariable(parentOpt(id, morpheus.getASTEnv)))

            logger.info("Run " + run + ": Variable IDs found: " + variableIds.size)

            if (variableIds.isEmpty && FORCE_VARIABILITY) {
                return null
            }

            def getRandomID(depth : Int = 0) : Id = {
                if (FORCE_VARIABILITY && variableIds.size < depth) return null
                val randID = if (FORCE_VARIABILITY && variableIds.nonEmpty) variableIds.apply((math.random * variableIds.size).toInt) else nonRefactoredIds.apply((math.random * ids.size).toInt)
                if (isWritable(randID)) randID
                else getRandomID(depth + 1)
            }

            val id = getRandomID()

            if (id == null) return null

            val associatedIds = morpheus.getReferences(id)
            addType(associatedIds, morpheus, run)
            logger.info("Run " + run + ": Found Id: " + id)
            (id, associatedIds.length, associatedIds.map(id => morpheus.getASTEnv.featureExpr(id.entry)).distinct)
        }

        val time = new StopClock
        val toRename = getVariableIdToRename
        if (toRename == null) return (false, null, List(), List())
        val determineTime = time.getTime
        logger.info("Run " + run + ": Time to determine id: " + time.getTime)
        StatsCan.addStat(morpheus.getFile, run, RandomRefactorDeterminationTime, determineTime)
        val id = toRename._1
        StatsCan.addStat(morpheus.getFile, run, RenamedId, id.name)

        val refactorChain = if (moduleInterface != null) getLinkedFilesToRefactor(moduleInterface, id)
        else List()

        if (refactorChain == null) return (false, null, List(), List())
        if (!refactorChain.isEmpty) {
            renameLink + name
            logger.info("Run " + run + ": Is linked.")
        } else logger.info("Run " + run + ": Is not linked.")

        val features = toRename._3
        StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, features)

        val startRenaming = new StopClock
        val refactored = CRenameIdentifier.rename(id, name, morpheus)
        val renamingTime = startRenaming.getTime
        logger.info("Run " + run + ": Renaming time : " + renamingTime)

        StatsCan.addStat(morpheus.getFile, run, RefactorTime, renamingTime)

        refactored match {
            case Right(ast) => {
                val linkedRefactored = refactorChain.map(x => {
                    val linkedId = findIdInAST(x._2, id, x._1.getTranslationUnit)
                    val time = new StopClock
                    val ref = CRenameIdentifier.rename(linkedId.get, name, x._1)
                    StatsCan.addStat(x._1.getFile, run, RefactorTime, time.getTime)
                    ref match {
                        case Right(refAST) => (x._1.getFile, refAST)
                        case Left(s) =>
                            logger.error("Run " + run + ": Refactoring failed at file " + x._1.getFile + " with " + s + ".")
                            return (false, null, List(), List())
                    }
                })
                logger.info("Run " + run + ": Refactoring at file " + morpheus.getFile + " successful.")
                (true, ast, features, linkedRefactored)
            }
            case Left(s) =>
                logger.error("Run " + run + ": Refactoring failed at file " + morpheus.getFile + " with " + s + ".")
                (false, null, List(), List())
        }

    }

    private def findIdInAST(position: Position, id: Id, tUnit: TranslationUnit) = {
        logger.info("Looking for " + position + "of " + id.name + ".")
        val found = filterASTElems[Id](tUnit).par.find(aId => {
            if (aId.name.equalsIgnoreCase(id.name)) logger.info("Found matching names " + id.name + " at: " + aId.getPositionFrom + ", " + aId.getPositionTo)
            // as positions in TypeChef are little bit buggy - we extend the search ranch.
            ((position.getLine.equals(aId.getPositionFrom.getLine) || position.getLine.equals(aId.getPositionTo.getLine)
                || position.getLine.equals(aId.getPositionFrom.getLine - 1) || position.getLine.equals(aId.getPositionTo.getLine - 1)
                || position.getLine.equals(aId.getPositionFrom.getLine + 1) || position.getLine.equals(aId.getPositionTo.getLine + 1))
                && aId.name.equalsIgnoreCase(id.name))
        })
        logger.info("Found the following linkedIds: " + found)
        found
    }


    private def getLinkedFilesToRefactor(modulInterface: CModuleInterface, id: Id): List[(Morpheus, Position)] = {
        val linked = modulInterface.getPositions(id.name)
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
                    case _ => logger.warn("Unknown Type " + id + " " + o)
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
