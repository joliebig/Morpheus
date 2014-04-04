package de.fosd.typechef.crefactor.evaluation.defaultEngines

import de.fosd.typechef.crefactor.evaluation._
import de.fosd.typechef.crefactor.{CRefactorFrontend, Morpheus}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.backend.engine.CRenameIdentifier
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import scala.collection.mutable
import de.fosd.typechef.error.Position
import java.io.File
import de.fosd.typechef.typesystem._
import de.fosd.typechef.conditional._
import java.util
import scala.Some
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.typesystem.CUnknown
import de.fosd.typechef.typesystem.CFunction
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.conditional.Opt
import scala.util.Random


trait DefaultRename extends Refactoring with Evaluation {

    val REFACTOR_NAME = "refactoredID"

    val REFACTOR_AMOUNT = 30

    private val renameLink = new util.HashSet[String]()

    private val linkedRenamedFiles = mutable.HashMap[String, Morpheus]()

    // refactor attempts to make REFACTOR_AMOUNT renamings in a file
    def refactor(morpheus: Morpheus): (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        var succ = false
        var runMorpheus = morpheus
        var affectedFeatures = List[List[FeatureExpr]]()

        for (run <- 1 to REFACTOR_AMOUNT) {
            val refactoredRun = singleRefactor(runMorpheus, run)

            if (refactoredRun._1) {
                succ = refactoredRun._1
                StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, refactoredRun._3)
                affectedFeatures = refactoredRun._3 :: affectedFeatures
                runMorpheus = new Morpheus(refactoredRun._2, morpheus.getFM, morpheus.getModuleInterface, morpheus.getFile)

                refactoredRun._4.foreach(
                    entry => linkedRenamedFiles.put(removeFilePrefix(entry._1), new Morpheus(entry._2, runMorpheus.getFM, removeFilePrefix(entry._1))))

                writeRunResult(run, runMorpheus, refactoredRun._4)
                logger.info("Run " + run + " affected features: " + refactoredRun._3)
            } else {
                logger.info("Run " + run + " failed.")
            }
        }

        (succ, runMorpheus.getTranslationUnit, affectedFeatures.distinct,
            linkedRenamedFiles.toList.map(entry => (removeFilePrefix(entry._1), entry._2.getTranslationUnit)))
    }

    // this function applies a single renaming, after checking different predicates
    // such as isValidId, ...
    private def singleRefactor(morpheus: Morpheus, run: Int):
    (Boolean, TranslationUnit, List[FeatureExpr], List[(String, TranslationUnit)]) = {
        val moduleInterface = morpheus.getModuleInterface
        val name = REFACTOR_NAME + "_" + run
        logger.info("+++ Start run: " + run)

        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            def isValidId(id: Id): Boolean = !id.name.contains("_main") && !isSystemLinkedName(id.name) && {
                if (moduleInterface != null) !(moduleInterface.isBlackListed(id.name) || renameLink.contains(id.name))
                else true
            } && !isExternalDeclWithNoLinkingInformation(id, morpheus)

            // TODO Fix Bug in OpenSSL for functions without body
            // We check the writable property here already in order to maximize the number of possible refactorings.
            def isWritable(id: Id): Boolean =
                morpheus.getReferences(id).map(_.entry).forall(i =>
                    isValidId(i) &&
                        (i.getFile.get.replaceFirst("file ", "").equalsIgnoreCase(morpheus.getFile) ||
                            new File(i.getFile.get.replaceFirst("file ", "")).canWrite))

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

            val randomIDs =
                if (FORCE_VARIABILITY) Random.shuffle(variableIds.toList)
                else Random.shuffle(nonRefactoredIds.toList)

            def getRandomID(randomIds : List[Id]) : Id = {
                logger.info(randomIds.size)
                randomIDs match {
                    case Nil => null
                    case headId :: tail =>
                        println(tail.size)
                        if (isWritable(headId)) headId
                        else getRandomID(tail)
                }
            }

            val id = getRandomID(randomIDs)
            logger.info("Run " + run + ": Found Id: " + id)
            if (id == null)
                return null

            val associatedIds = morpheus.getReferences(id)
            addType(associatedIds, morpheus, run)
            (id, associatedIds.length, associatedIds.map(id => morpheus.getASTEnv.featureExpr(id.entry)).distinct)
        }

        val time = new StopClock
        val toRename = getVariableIdToRename
        if (toRename == null)
            return (false, null, List(), List())

        val determineTime = time.getTime
        logger.info("Run " + run + ": Time to determine id: " + time.getTime)
        StatsCan.addStat(morpheus.getFile, run, RandomRefactorDeterminationTime, determineTime)
        val id = toRename._1
        StatsCan.addStat(morpheus.getFile, run, RenamedId, id.name)

        val refactorChain = if (moduleInterface != null)
                                getLinkedFilesToRefactor(morpheus, id)
                            else
                                List()

        if (refactorChain == null)
            return (false, null, List(), List())

        if (!refactorChain.isEmpty) {
            renameLink + name
            logger.info("Run " + run + ": Is linked.")
        } else
            logger.info("Run " + run + ": Is not linked.")

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
            ((position.getLine.equals(aId.getPositionFrom.getLine) ||
                position.getLine.equals(aId.getPositionTo.getLine) ||
                position.getLine.equals(aId.getPositionFrom.getLine - 1) ||
                position.getLine.equals(aId.getPositionTo.getLine - 1) ||
                position.getLine.equals(aId.getPositionFrom.getLine + 1) ||
                position.getLine.equals(aId.getPositionTo.getLine + 1))
                && aId.name.equalsIgnoreCase(id.name))
        })
        logger.info("Found the following linkedIds: " + found)
        found
    }


    private def getLinkedFilesToRefactor(morpheus: Morpheus, id: Id): List[(Morpheus, Position)] = {
        val cmif = morpheus.getModuleInterface
        val linked = cmif.getPositions(id.name)

        val affectedFiles = linked.foldLeft(new mutable.HashMap[String, Position])((map, pos) =>
            if (getFileName(pos.getFile).equalsIgnoreCase(getFileName(morpheus.getFile))) map
            else map += (pos.getFile -> pos))

        if (affectedFiles.keySet.exists(file => blackListFiles.exists(getFileName(file).equalsIgnoreCase)
            || (!evalFiles.exists(getFileName(file).equalsIgnoreCase)))) {
            logger.info("One or more file is blacklisted or is not a member of the valid files list and cannot be build.")
            return null
        }

        val refactorChain = affectedFiles.foldLeft(List[(Morpheus, Position)]())((list, entry) => {
            linkedRenamedFiles.get(removeFilePrefix(entry._1)) match {
                case Some(morpheus) =>
                    list :+(morpheus, entry._2)
                case _ =>
                    val linked = CRefactorFrontend.parseOrLoadTUnit(removeFilePrefix(entry._1))
                    list :+(new Morpheus(linked._1, linked._2, removeFilePrefix(entry._1)), entry._2)
            }
        })

        refactorChain
    }

    private def addType(ids: List[Opt[Id]], morpheus: Morpheus, run: Int) = {
        def traverseTypesAndCount(c: Conditional[_], id: Id) = {
            val tautTypes = ConditionalLib.items(c).filter { entry => entry._1.isTautology(morpheus.getFM) }
            tautTypes.map(_._2).flatMap({
                case c@(CUnknown(_), _, _) =>
                    logger.warn("Is unkown " + id + " " + c)
                    None
                case (CFunction(_, _), _, _) => Some("FunctionName")
                case (CType(CFunction(_, _), _, _, _), _, _, _) => Some("FunctionName")
                case (_, KEnumVar, _, _) => Some("Enum")
                case (CType(_, _, _, _), _, _, _) => Some("Variable")
                case o =>
                    logger.warn("Unknown Type " + id + " " + o)
                    None
            })
        }
        val foundTypes = ids.flatMap(id => {
            try {
                // only lookup variables
                traverseTypesAndCount(morpheus.getEnv(id.entry).varEnv.lookup(id.entry.name), id.entry)
            } catch {
                case _: Throwable => Some("TypeDef")
            }
        })

        StatsCan.addStat(morpheus.getFile, run, Type, foundTypes)
    }
}
