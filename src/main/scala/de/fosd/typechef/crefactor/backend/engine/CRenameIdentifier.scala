package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.crefactor.backend.{CRefactor, ASTSelection}
import de.fosd.typechef.parser.c.{TranslationUnit, AST, Id}
import de.fosd.typechef.crefactor.frontend.util.Selection
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import de.fosd.typechef.crefactor.evaluation.StatsJar
import de.fosd.typechef.crefactor.evaluation.Stats._
import java.io.File
import de.fosd.typechef.conditional.Opt

/**
 * Implements the technique of correctly renaming an identifier.
 */
object CRenameIdentifier extends ASTSelection with CRefactor {

    def getSelectedElements(morpheus: Morpheus, selection: Selection): List[AST] = getAvailableIdentifiers(morpheus, selection)

    def getAvailableIdentifiers(morpheus: Morpheus, selection: Selection): List[Id] =
        filterASTElems[Id](morpheus.getTranslationUnit).
            par.filter(x => isPartOfSelection(x, selection)).
            toList.filter(x => isPartOfFile(x, selection.getFilePath))

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean =
        !getAvailableIdentifiers(morpheus, selection).isEmpty

    def rename(id: Id, nid: String, morpheus: Morpheus): Either[String, TranslationUnit] = {
        val lid = morpheus.linkage(id).map(_.entry)
        StatsJar.addStat(morpheus.getFile, Amount, lid.size)

        if (!isValidId(nid))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (lid.exists(cid => isLinked(Opt(parentOpt(cid, morpheus.getASTEnv).feature, nid), morpheus)))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (lid.exists(isShadowed(nid, _, morpheus)))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.shadowing"))
        else if (!lid.par.forall(id => new File(id.getFile.get.replaceFirst("file ", "")).canWrite))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.rename"))
        else
            Right(replaceIds(morpheus.getTranslationUnit, lid, nid))
    }
}
