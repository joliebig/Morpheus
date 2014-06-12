package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.crefactor.backend.CRefactor
import de.fosd.typechef.parser.c.{TranslationUnit, Id}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import de.fosd.typechef.conditional.Opt

/**
 * Implements the technique of correctly renaming an identifier.
 */
object CRenameIdentifier extends CRefactor {

    // TODO Move in rename
    def canRefactor(id: Id, morpheus: Morpheus,
                    validIdForRenameInCaseStudy: (Id, Morpheus) => Boolean = (i, m) => true) : Boolean = {
        val rid = morpheus.getReferences(id).map(_.entry)
        
        if (!isValidIdForRename(id, morpheus, validIdForRenameInCaseStudy))
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
        else if (!rid.forall { // TODO: could be limited to external declarations
          r =>
            val ropt = Opt(morpheus.getASTEnv.featureExpr(parentOpt(r, morpheus.getASTEnv)), nid)
            isValidInProgram(ropt, morpheus)
        })
          Left(Configuration.getInstance().getConfig("default.error.invalidName"))
        else if (!rid.forall(isValidInModule(nid, _, morpheus)))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.shadowing"))
        // isWritable - with workaround for openssl casestudy with incomplete paths
        else if (!rid.par.forall(isWritable(_, morpheus)))
            Left(Configuration.getInstance().getConfig("engine.rename.failed.rename"))
        else
            Right(replaceIds(morpheus.getTranslationUnit, rid, nid))
    }
}
