package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.crefactor.backend.{CRefactor, ASTSelection}
import de.fosd.typechef.parser.c.{TranslationUnit, AST, Id}
import de.fosd.typechef.crefactor.frontend.util.Selection
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import de.fosd.typechef.crefactor.evaluation.StatsJar
import de.fosd.typechef.crefactor.evaluation.Stats._
import java.io.File

/**
 * Implements the technique of correctly renaming an identifier.
 */
object CRenameIdentifier extends ASTSelection with CRefactor {

    def getSelectedElements(morpheus: Morpheus, selection: Selection): List[AST]
    = getAvailableIdentifiers(morpheus, selection)

    def getAvailableIdentifiers(morpheus: Morpheus, selection: Selection): List[Id] =
        filterASTElems[Id](morpheus.getTranslationUnit).
            par.filter(x => isPartOfSelection(x, selection)).
            toList.filter(x => isPartOfFile(x, selection.getFilePath))

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean =
        !getAvailableIdentifiers(morpheus, selection).isEmpty

    def rename(id: Id, nid: String, morpheus: Morpheus): Either[String, TranslationUnit] = {
        val lid = morpheus.getReferences(id).map(_.entry)
        StatsJar.addStat(morpheus.getFile, Amount, lid.size)

        if (!isValidId(nid))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (isValidInProgram(nid, morpheus))
            Left(Configuration.getInstance().getConfig("default.error.isInConflictWithSymbolInModuleInterface"))
        else if (lid.exists(isValidInModule(nid, _, morpheus)))
            Left(Configuration.getInstance().getConfig("default.error.isInConflictWithSymbolInModule"))
        else if (!lid.par.forall(id => new File(id.getFile.get.replaceFirst("file ", "")).canWrite))
            Left(Configuration.getInstance().getConfig("refactor.rename.failed.writing"))
        else
            Right(replaceIds(morpheus.getTranslationUnit, lid, nid))
    }
}
