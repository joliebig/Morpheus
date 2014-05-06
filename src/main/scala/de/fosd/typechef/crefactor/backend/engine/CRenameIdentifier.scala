package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.crefactor.backend.{CRefactor, ASTSelection}
import de.fosd.typechef.parser.c.{TranslationUnit, AST, Id}
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import java.io.File
import de.fosd.typechef.conditional.Opt

/**
 * Implements the technique of correctly renaming an identifier.
 */
object CRenameIdentifier extends ASTSelection with CRefactor {

    // TODO @ajanker: Do the following three functions belong here?
    def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST] =
        getAvailableIdentifiers(morpheus, selection)

    def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id] =
        filterASTElems[Id](morpheus.getTranslationUnit).
            par.filter(x => isPartOfSelection(x, selection))
            .filter(x => isPartOfFile(x, selection.getFilePath)).toList

    def isAvailable(morpheus: Morpheus, selection: CodeSelection): Boolean =
        !getAvailableIdentifiers(morpheus, selection).isEmpty

    def rename(id: Id, nid: String, morpheus: Morpheus): Either[String, TranslationUnit] = {
        val rid = morpheus.getReferences(id).map(_.entry)

        if (!isValidId(nid))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (isValidInProgram(Opt(parentOpt(id, morpheus.getASTEnv).feature, nid), morpheus))
            Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (rid.exists(isValidInModule(nid, _, morpheus)))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.shadowing"))
        // isWritable - with workaround for openssl casestudy with incomplete paths
        else if (!rid.par.forall(id => {
            val path =
                if (hasSameFileName(id, morpheus)) morpheus.getFile
                else id.getFile.get.replaceFirst("file ", "")
            new File(path).canWrite
        }))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.rename"))
        else
            Right(replaceIds(morpheus.getTranslationUnit, rid, nid))
    }

    private def hasSameFileName(id : Id, morpheus : Morpheus) : Boolean = {
        val entry = id.getFile.get.replaceFirst("file ", "")
        entry.equalsIgnoreCase(morpheus.getFile) ||
            getFileName(entry).equalsIgnoreCase(getFileName(morpheus.getFile))
    }

    private def getFileName(originalFilePath: String) =
        if (originalFilePath.contains(File.separatorChar))
            originalFilePath.substring(originalFilePath.lastIndexOf(File.separatorChar),
                originalFilePath.length).replace("/", "")
        else originalFilePath
}
