package de.fosd.typechef.crefactor.backend.refactor

import de.fosd.typechef.crefactor.backend.ASTSelection
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.frontend.util.Selection
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import de.fosd.typechef.crefactor.evaluation.StatsJar
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.parser.c.Id

/**
 * Implements the technique of correctly renaming an identifier.
 */
object CRenameIdentifier extends ASTSelection with CRefactor {

    def getSelectedElements(morpheus: Morpheus, selection: Selection): List[AST] = getAvailableIdentifiers(morpheus, selection)

    def getAvailableIdentifiers(morpheus: Morpheus, selection: Selection): List[Id] =
        filterASTElems[Id](morpheus.getAST).par.filter(x => isInSelectionRange(x, selection)).toList.filter(x => isElementOfFile(x, selection.getFilePath))

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean = !getAvailableIdentifiers(morpheus, selection).isEmpty

    def rename(id: Id, newName: String, morpheus: Morpheus): Either[String, AST] = {
        val idsToRename = getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
        StatsJar.addStat(morpheus.getFile, Amount, idsToRename.size)
        if (!isValidName(newName)) Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (idsToRename.exists(isShadowed(newName, _, morpheus))) Left(Configuration.getInstance().getConfig("refactor.rename.failed.shadowing"))
        else Right(renameIDsInAST(morpheus.getAST, idsToRename, newName))
    }
}
