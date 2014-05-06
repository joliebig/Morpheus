package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.crefactor.backend.CRefactor
import de.fosd.typechef.parser.c.{TranslationUnit, AST, Id}
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import java.io.File
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crefactor.backend.codeselection.ASTSelection

/**
 * Implements the technique of correctly renaming an identifier.
 */
object CRenameIdentifier extends ASTSelection with CRefactor {

    // TODO @ajanker: Do the following three functions belong here?
    def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST] =
        getAvailableIdentifiers(morpheus, selection)

    // TODO Move
    def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id] =
        filterASTElems[Id](morpheus.getTranslationUnit).
            par.filter(x => isPartOfSelection(x, selection))
            .filter(x => isPartOfFile(x, selection.getFilePath)).toList

    // TODO ToRemove
    def isAvailable(morpheus: Morpheus, selection: CodeSelection): Boolean =
        !getAvailableIdentifiers(morpheus, selection).isEmpty

    
    def canRefactor(id: Id, morpheus: Morpheus) : Boolean = {
        val rid = morpheus.getReferences(id).map(_.entry)
        
        if (!isValidForRename(id, morpheus))
            false
        else if (!rid.par.forall(isWritable(_, morpheus)))
            false
        else
            true
    }

    def rename(id: Id, nid: String, morpheus: Morpheus): Either[String, TranslationUnit] = {
        val rid = morpheus.getReferences(id).map(_.entry)

        // TODO Compare with canRefactor
        if (!isValidName(nid))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (isValidInProgram(Opt(parentOpt(id, morpheus.getASTEnv).feature, nid), morpheus))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (rid.exists(isValidInModule(nid, _, morpheus)))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.shadowing"))
        // isWritable - with workaround for openssl casestudy with incomplete paths
        else if (!rid.par.forall(isWritable(_, morpheus)))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.rename"))
        else
            Right(replaceIds(morpheus.getTranslationUnit, rid, nid))
    }
}
