package de.fosd.typechef.crefactor.evaluation.defaultEngines

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

trait DefaultRenameEngine extends Refactoring with Evaluation {

    val REFACTOR_NAME = "refactoredID"

    val REFACTOR_AMOUNT = 1

    private val renameLink = new util.HashSet[String]()

    private val linkedRenamedFiles = mutable.HashMap[String, Morpheus]()

    // not supported
    override def getValidStatementsForEvaluation(morpheus: Morpheus): List[List[Statement]] = List()

    override def getValidIdsForEvaluation(morpheus : Morpheus) : List[Id] = {
        val moduleInterface = morpheus.getModuleInterface

        val allUses = morpheus.getAllUses.par.filter(hasSameFileName(_, morpheus))
        val availableIds = allUses.toList.filter(CRenameIdentifier.canRefactor(_, morpheus, isValidIdForRename))

        val linkedIds = if (FORCE_LINKING && moduleInterface != null)
            availableIds.par.filter(id =>
                moduleInterface.isListed(Opt(parentOpt(id, morpheus.getASTEnv).feature, id.name), morpheus.getFM))
                        else availableIds

        val ids = if (linkedIds.isEmpty) availableIds else linkedIds.toList
        logger.info(morpheus.getFile + ": IDs found: " + ids.size)

        val variableIds = ids.filter(id => isVariable(id, morpheus.getASTEnv))
        logger.info(morpheus.getFile  + ": Variable IDs found: " + variableIds.size)

        val randomIDs =
            if (FORCE_VARIABILITY && !variableIds.isEmpty) Random.shuffle(variableIds.toList)
            else if (FORCE_VARIABILITY) List()
            else Random.shuffle(ids.toList)

        val validIDs =
            if (moduleInterface == null) randomIDs
            else randomIDs.filter(canLink(_, morpheus))

        logger.info(morpheus.getFile  + ": IDs to rename found: " + validIDs.size)

        validIDs.toList
    }

    // refactor attempts to make REFACTOR_AMOUNT renamings in a file
    def refactor(morpheus: Morpheus, preparedRefs : PreparedRefactorings):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        var succ = false
        var runMorpheus = morpheus
        var affectedFeatures = List[List[FeatureExpr]]()
        var preparedRefactorings = preparedRefs

        for (run <- 1 to REFACTOR_AMOUNT) {
            val (refResult, refTUnit, refAffectedFeaturs, refAffectedFiles, refPrepared) =
                refactorRun(run, runMorpheus, preparedRefactorings)

            if (refResult) {
                succ = refResult
                StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, refAffectedFeaturs)
                affectedFeatures ::= refAffectedFeaturs
                runMorpheus = new Morpheus(refTUnit, morpheus.getFM, morpheus.getModuleInterface, morpheus.getFile)
                preparedRefactorings = refPrepared

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

    private def refactorRun(run : Int, morpheus : Morpheus, prepared : PreparedRefactorings) :
    (Boolean, TranslationUnit, List[FeatureExpr], List[(String, TranslationUnit)], PreparedRefactorings) = {
        logger.info("+++ Start run: " + run)

        if (prepared.renaming.isEmpty)
            return (false, null, List(), List(), null)

        val moduleInterface = morpheus.getModuleInterface
        val name = REFACTOR_NAME + "_" + run

        val preparedId = prepared.renaming.head

        val id = prepared.getCorrespondingId(preparedId, morpheus) match {
            case Some(id) => id
            case _ => null
        }

        if (id == null)
            return (false, null, List(), List(), null)

        StatsCan.addStat(morpheus.getFile, run, RenamedId, id.name)

        val refactorChain =
            if (moduleInterface != null)
                getRefactoringObjectsForGloballyLinkedIdentifiers(morpheus, id)
            else
                List()

        if (refactorChain == null)
            return (false, null, List(), List(), null)

        if (!refactorChain.isEmpty) {
            renameLink + name
            logger.info("Run " + run + ": Is linked.")
        } else
            logger.info("Run " + run + ": Is not linked.")

        val startRenaming = new StopClock
        val refactored = CRenameIdentifier.rename(id, name, morpheus)
        val renamingTime = startRenaming.getTime

        StatsCan.addStat(morpheus.getFile, run, RefactorTime, renamingTime)

        refactored match {
            case Right(ast) => {
                val refLinkedFiles = refactorChain.map(entry => {
                    // reuse or create morpheus (refactoring) objects
                    val chainEntry = entry match {
                        case (fName, fPos) =>
                            val fNameNoPrefix = removeFilePrefix(fName)
                            linkedRenamedFiles.get(fNameNoPrefix) match {
                                case Some(m) =>
                                    logger.info(fNameNoPrefix + "is cached.")
                                    (m, fPos)
                                case _ =>
                                    // TODO Check with duplicate short option exception
                                    logger.info("Loading: " + fNameNoPrefix.replace(".c", ".tunit"))
                                    val tu= CRefactorFrontend.loadSerializedTUnit(fNameNoPrefix.replace(".c", ".tunit"))
                                    (new Morpheus(tu, morpheus.getFM, fNameNoPrefix), fPos)
                            }
                    }

                    val linkedId = findIdInAST(chainEntry._2, id.name, chainEntry._1.getTranslationUnit)
                    val time = new StopClock
                    val ref = CRenameIdentifier.rename(linkedId.get, name, chainEntry._1)
                    val refTime = time.getTime
                    ref match {
                        case Right(refAST) => {
                            val references = chainEntry._1.getReferences(linkedId.get).length
                            StatsCan.addStat(chainEntry._1.getFile, run, RefactorTime, refTime)
                            StatsCan.addStat(chainEntry._1.getFile, run, RenamedId, id.name)
                            StatsCan.addStat(chainEntry._1.getFile, run, Amount, references)
                            logger.info("Run " + run + ": Renaming time : " + renamingTime + "for linked file: " + chainEntry._1.getFile)
                            logger.info("Run " + run + ": Id : " + id.name + "for linked file: " + chainEntry._1.getFile)
                            logger.info("Run " + run + ": References : " + references + "for linked file: " + chainEntry._1.getFile)
                            (chainEntry._1.getFile, refAST)
                        }
                        case Left(s) =>
                            logger.error("Run " + run + ": Refactoring failed at file " + chainEntry._1.getFile + " with " + s + ".")
                            return (false, null, List(), List(), null)
                    }
                })

                val associatedIds = morpheus.getReferences(id)
                val features = associatedIds.map(id => morpheus.getASTEnv.featureExpr(id.entry)).distinct
                countAndLogIdTypes(associatedIds, morpheus, run)
                StatsCan.addStat(morpheus.getFile, run, AffectedFeatures, features)
                StatsCan.addStat(morpheus.getFile, run, Amount, associatedIds.size)
                logger.info("Run " + run + ": Ids : " + associatedIds.size)
                logger.info("Run " + run + ": Renaming time : " + renamingTime)
                logger.info("Run " + run + ": Refactoring at file " + morpheus.getFile + " successful.")
                val newRenaming = prepared.renaming.par.filterNot(pId =>
                        prepared.getCorrespondingId(pId, morpheus) match {
                            case Some(cId) => associatedIds.exists(aId => aId.entry.eq(cId))
                            case _ => false
                        })
                (true, ast, features, refLinkedFiles, prepared.copy(renaming = newRenaming.toList))
            }
            case Left(s) =>
                logger.error("Run " + run + ": Refactoring failed at file " + morpheus.getFile + " with " + s + ".")
                (false, null, List(), List(), null)
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
    private def getRefactoringObjectsForGloballyLinkedIdentifiers(morpheus: Morpheus, id: Id): List[(String, Position)] = {
        val cmif = morpheus.getModuleInterface
        val linked = cmif.getPositions(id.name)
        val linkedFiles = util.Collections.newSetFromMap[String](new util.IdentityHashMap)

        // get files with linking different from the current file, we already operate on
        val affectedFiles = linked.flatMap {
            pos => {
                if (getFileName(pos.getFile).equalsIgnoreCase(getFileName(morpheus.getFile))
                    || linkedFiles.contains(pos.getFile))
                    None
                else  {
                    linkedFiles.add(pos.getFile)
                    Some((pos.getFile, pos))
                }
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
        logger.info("Affected files: " + affectedFiles)
        affectedFiles
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
}
