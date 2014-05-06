package de.fosd.typechef.crefactor.backend.codeselection

import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.parser.c.{Id, AST}

object CRenameSelection extends ASTSelection {

    override def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST] =
        getAvailableIdentifiers(morpheus, selection)

    override def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id] =
        filterASTElems[Id](morpheus.getTranslationUnit).
            par.filter(x => isPartOfSelection(x, selection))
            .filter(x => isPartOfFile(x, selection.getFilePath)).toList
}
