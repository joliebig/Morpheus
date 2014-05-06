package de.fosd.typechef.crefactor.evaluation.defaultEngines

import java.io.File
import java.util
import scala.collection.mutable
import scala.util.Random

import de.fosd.typechef.conditional._
import de.fosd.typechef.crefactor.evaluation._
import de.fosd.typechef.crefactor.{CRefactorFrontend, Morpheus}
import de.fosd.typechef.crefactor.backend.engine.CRenameIdentifier
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.error.Position
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.{Statement, Id, TranslationUnit}
import de.fosd.typechef.typesystem._

trait DefaultRename extends Refactoring with Evaluation {

    val REFACTOR_NAME = "refactoredID"

    val REFACTOR_AMOUNT = 30

    private val renameLink = new util.HashSet[String]()

    private val linkedRenamedFiles = mutable.HashMap[String, Morpheus]()

    // not supported
    def getValidStatementsForEvaluation(morpheus: Morpheus): List[Statement] = List()

    def getValidIdsForEvaluation(morpheus : Morpheus) : List[Id] = {
        val moduleInterface = morpheus.getModuleInterface

        // We check if an id has an valid name, is neither blacklisted, or called main or has wrong linking informations
        def isValidId(id: Id): Boolean =
            !id.name.contains("_main") && !isSystemLinkedName(id.name) && {
                if (moduleInterface != null)
                    !(moduleInterface.isBlackListed(id.name) || renameLink.contains(id.name))
                else true
                } && !isExternalDeclWithNoLinkingInformation(id, morpheus) &&
                        id.hasPosition && !isOnlyLocallyLinked(id, morpheus)

        // We check the writable property here already in order to maximize the number of possible refactorings.
        def isWritable(id: Id): Boolean =
            morpheus.getReferences(id).map(_.entry).forall(i =>
                isValidId(i) &&
                    (hasSameFileName(i, morpheus) || new File(i.getFile.get.replaceFirst("file ", "")).canWrite))

        val allIds = morpheus.getAllUses.filter(isWritable)

        val linkedIds = if (FORCE_LINKING && moduleInterface != null)
            allIds.par.filter(id => moduleInterface.isListed(Opt(parentOpt(id, morpheus.getASTEnv).feature, id.name), morpheus.getFM))
                        else allIds

        val ids = if (linkedIds.isEmpty) allIds else linkedIds
        logger.info(morpheus.getFile + ": IDs found: " + ids.size)

        val variableIds = ids.par.filter(id => isVariable(parentOpt(id, morpheus.getASTEnv)))
        logger.info(morpheus.getFile  + ": Variable IDs found: " + variableIds.size)

        val randomIDs =
            if (FORCE_VARIABILITY && !variableIds.isEmpty) Random.shuffle(variableIds)
            else if (FORCE_VARIABILITY) List()
            else Random.shuffle(ids)

        val validIDs =
            if (moduleInterface == null) randomIDs
            else randomIDs.filter(canLink(_, morpheus))

        logger.info(morpheus.getFile  + ": IDs to rename found: " + validIDs.size)

        validIDs.toList
    }

    // refactor attempts to make REFACTOR_AMOUNT renamings in a file
    def refactor(morpheus: Morpheus): (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        var succ = false
        var runMorpheus = morpheus
        var affectedFeatures = List[List[FeatureExpr]]()

        for (run <- 1 to REFACTOR_AMOUNT) {
            val (refResult, refTUnit, refAffectedFeaturs, refAffectedFiles) = singleRefactor(runMorpheus, run)

            if (refResult) {
                succ = refResult
                StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, refAffectedFeaturs)
                affectedFeatures ::= refAffectedFeaturs
                runMorpheus = new Morpheus(refTUnit, morpheus.getFM, morpheus.getModuleInterface, morpheus.getFile)

                refAffectedFiles.foreach {
                    case (affFName, affTUnit) =>
                        val noPrefix = removeFilePrefix(affFName)
                        linkedRenamedFiles.put(noPrefix, new Morpheus(affTUnit, runMorpheus.getFM, noPrefix))
                }

                writeRunResult(run, runMorpheus, refAffectedFiles)
                logger.info("Run " + run + " affected features: " + refAffectedFeaturs)
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
                if (moduleInterface != null)
                    !(moduleInterface.isBlackListed(id.name) || renameLink.contains(id.name))
                else true
            } && !isExternalDeclWithNoLinkingInformation(id, morpheus) &&
                id.hasPosition && !isOnlyLocallyLinked(id, morpheus)

            morpheus.getTypeSystem.getInferredInterface().exports.exists(sig => sig.name.equals())

            // We check the writable property here already in order to maximize the number of possible refactorings.
            def isWritable(id: Id): Boolean =
                morpheus.getReferences(id).map(_.entry).forall(i =>
                    isValidId(i) &&
                        (hasSameFileName(i, morpheus) || new File(i.getFile.get.replaceFirst("file ", "")).canWrite))
            val allIds = morpheus.getAllUses.par.filter(hasSameFileName(_, morpheus))

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

            def getRandomID(ids : List[Id]) : Id = {
                ids match {
                    case Nil => null
                    case headId :: tail =>
                        if (isWritable(headId)) headId
                        else getRandomID(tail)
                }
            }

            val id = getRandomID(randomIDs)
            logger.info("Run " + run + ": Found Id: " + id)
            if (id == null)
                return null

            val associatedIds = morpheus.getReferences(id)
            countAndLogIdTypes(associatedIds, morpheus, run)
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
                                getRefactoringObjectsForGloballyLinkedIdentifiers(morpheus, id)
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

        val startRenaming = new StopClock
        val refactored = CRenameIdentifier.rename(id, name, morpheus)
        val renamingTime = startRenaming.getTime

        StatsCan.addStat(morpheus.getFile, run, RefactorTime, renamingTime)

        refactored match {
            case Right(ast) => {
                val linkedRefactored = refactorChain.map(x => {
                    val linkedId = findIdInAST(x._2, id.name, x._1.getTranslationUnit)
                    val time = new StopClock
                    val ref = CRenameIdentifier.rename(linkedId.get, name, x._1)
                    val refTime = time.getTime
                    ref match {
                        case Right(refAST) => {
                            val references = x._1.getReferences(linkedId.get).length
                            StatsCan.addStat(x._1.getFile, run, RefactorTime, refTime)
                            StatsCan.addStat(x._1.getFile, run, RenamedId, id.name)
                            StatsCan.addStat(x._1.getFile, run, Amount, references)
                            logger.info("Run " + run + ": Renaming time : " + renamingTime + "for linked file: " + x._1.getFile)
                            logger.info("Run " + run + ": Id : " + id.name + "for linked file: " + x._1.getFile)
                            logger.info("Run " + run + ": References : " + references + "for linked file: " + x._1.getFile)
                            (x._1.getFile, refAST)
                        }
                        case Left(s) =>
                            logger.error("Run " + run + ": Refactoring failed at file " + x._1.getFile + " with " + s + ".")
                            return (false, null, List(), List())
                    }
                })
                StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, features)
                StatsCan.addStat(morpheus.getFile, run, Amount, toRename._2)
                logger.info("Run " + run + ": Renaming time : " + renamingTime)
                logger.info("Run " + run + ": Refactoring at file " + morpheus.getFile + " successful.")
                (true, ast, features, linkedRefactored)
            }
            case Left(s) =>
                logger.error("Run " + run + ": Refactoring failed at file " + morpheus.getFile + " with " + s + ".")
                (false, null, List(), List())
        }

    }

    // checks if all possible linked reference of an id can be linked if necessary
    private def canLink(id: Id, morpheus : Morpheus) : Boolean = {
        val cmif = morpheus.getModuleInterface
        val linked = cmif.getPositions(id.name)

        // get files with linking different from the current file, we already operate on
        val affectedFiles = linked.flatMap {
            pos => {
                if (getFileName(pos.getFile).equalsIgnoreCase(getFileName(morpheus.getFile)))
                    None
                else
                    Some((pos.getFile, pos))
            }
        }

        // erroneous linking information?
        if (affectedFiles.isEmpty && linked.nonEmpty) {
            logger.info(id + "  is recognized as to be linked, but no corresponding file was found.")
            false
        }
        // we cannot handle all files in TypeChef
        else if (affectedFiles.exists(lPos => blackListFiles.exists(getFileName(lPos._1).equalsIgnoreCase)
            || (!evalFiles.exists(getFileName(lPos._1).equalsIgnoreCase)))) {
            logger.info(id + ": One or more file is blacklisted or is not a member of the valid files list and cannot be build.")
            false
        }
        else true
    }

    // given the position and the string of an identifier, the function returns, if the available, the
    // id object in the translation unit
    private def findIdInAST(position: Position, name: String, tUnit: TranslationUnit) = {
        logger.info("Looking for " + position + "of " + name + ".")
        val found = filterASTElems[Id](tUnit).par.find(aId => {
            if (aId.name.equalsIgnoreCase(name))
                logger.info("Found matching names " + name +
                    " at: " + aId.getPositionFrom + ", " + aId.getPositionTo)

            // as positions in TypeChef are a little bit buggy, we extend the search range.
            ((position.getLine.equals(aId.getPositionFrom.getLine) ||
                position.getLine.equals(aId.getPositionTo.getLine) ||
                position.getLine.equals(aId.getPositionFrom.getLine - 1) ||
                position.getLine.equals(aId.getPositionTo.getLine - 1) ||
                position.getLine.equals(aId.getPositionFrom.getLine + 1) ||
                position.getLine.equals(aId.getPositionTo.getLine + 1))
                && aId.name.equalsIgnoreCase(name))
        })
        logger.info("Found the following linkedIds: " + found)
        found
    }

    // get for each globally linked identifier the file and code position
    // (row and column) and create morpheus refactoring objects of them
    // we use the result subsequently to rename the linked identifiers also
    private def getRefactoringObjectsForGloballyLinkedIdentifiers(morpheus: Morpheus, id: Id): List[(Morpheus, Position)] = {
        val cmif = morpheus.getModuleInterface
        val linked = cmif.getPositions(id.name)

        // get files with linking different from the current file, we already operate on
        val affectedFiles = linked.flatMap {
            pos => {
                if (getFileName(pos.getFile).equalsIgnoreCase(getFileName(morpheus.getFile)))
                    None
                else
                    Some((pos.getFile, pos))
            }
        }

        // erroneous linking information?
        if (affectedFiles.isEmpty && linked.nonEmpty) {
            logger.info("Id is recognized as to be linked, but no corresponding file was found.")
            return null
        }

        // we cannot handle all files in TypeChef
        if (affectedFiles.exists(lPos => blackListFiles.exists(getFileName(lPos._1).equalsIgnoreCase)
            || (!evalFiles.exists(getFileName(lPos._1).equalsIgnoreCase)))) {
            logger.info("One or more file is blacklisted or is not a member of the valid files list and cannot be build.")
            return null
        }

        // reuse or create morpheus (refactoring) objects
        val refactorChain = affectedFiles.map {
            case (fName, fPos) =>
                val fNameNoPrefix = removeFilePrefix(fName)
                linkedRenamedFiles.get(fNameNoPrefix) match {
                    case Some(m) => (m, fPos)
                    case _ =>
                        val (tu, fm) = CRefactorFrontend.getTUnit(fNameNoPrefix)
                        (new Morpheus(tu, fm, fNameNoPrefix), fPos)
                }
        }

        refactorChain
    }

    // count and log renamed types of renamed ids
    private def countAndLogIdTypes(ids: List[Opt[Id]], morpheus: Morpheus, run: Int) = {
        var res: Map[String, Int] = Map()
        res += "Unknown"  -> 0
        res += "Function" -> 0
        res += "Enum"     -> 0
        res += "Variable" -> 0
        res += "TypeDef"  -> 0

        // an identifier may have different types depending on the configuration
        def traverseIdTypes(c: Conditional[_], id: Id) = {
            val satTypes = ConditionalLib.items(c).filter { entry => entry._1.isSatisfiable(morpheus.getFM) }

            satTypes.map(_._2).foreach({
                case c@(CUnknown(_), _, _) =>
                    logger.warn("Is unknown " + id + " " + c)
                    res += "Unknown" -> (res("Unknown") + 1)
                case (CFunction(_, _), _, _)                    => res += "Function" -> (res("Function") + 1)
                case (CType(CFunction(_, _), _, _, _), _, _, _) => res += "Function" -> (res("Function") + 1)
                case (_, KEnumVar, _, _)                        => res += "Enum"     -> (res("Enum") + 1)
                case (CType(_, _, _, _), _, _, _)               => res += "Variable" -> (res("Variable") + 1)
                case o =>
                    logger.warn("Unknown Type " + id + " " + o)
                    res += "Unknown" -> (res("Unknown") + 1)
            })
        }

        ids.foreach(id => {
            try {
                // only lookup variables
                traverseIdTypes(morpheus.getEnv(id.entry).varEnv.lookup(id.entry.name), id.entry)
            } catch {
                case _: Throwable => res += "TypeDef" -> (res("TypeDef") + 1)
            }
        })

        StatsCan.addStat(morpheus.getFile, run, Type, res)
    }

    // TODO @ajanker: What is the purpose of this function?
    // this functions detects if the local interface knows about linking while the global
    // interface does not know about it
    // TODO @ajanker: This sounds like an error in the global interface generation, right?
    // Or is there a specific reason, why a symbol (identifier) has local, but externally visible, binding
    // and is not in the global linking interface?
    def isOnlyLocallyLinked(id: Id, morpheus : Morpheus) = {
        val exports = morpheus.getTypeSystem.getInferredInterface().exports
        val imports = morpheus.getTypeSystem.getInferredInterface().imports

        val local = exports.exists(_.name == id.name) ||
            imports.exists(_.name == id.name)

        val global = if (morpheus.getModuleInterface == null)
                         false
                     else
                         morpheus.getModuleInterface.nameIsListed(id.name)

        local && !global
    }
}
