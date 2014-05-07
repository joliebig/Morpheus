package de.fosd.typechef.crefactor.backend.codeselection

import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.parser.c._
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.FunctionCall
import de.fosd.typechef.parser.c.NestedFunctionDef
import de.fosd.typechef.crefactor.backend.engine.CInlineFunction

object CInlineSelection extends ASTSelection {
    override def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST] = {
        val functions = (filterASTElems[FunctionDef](morpheus.getTranslationUnit) :::
            filterASTElems[FunctionCall](morpheus.getTranslationUnit) :::
            filterASTElems[NestedFunctionDef](morpheus.getTranslationUnit)).filter(isSelected(_, morpheus.getASTEnv, selection))

        filterASTElementsForFile(functions.map(getFunctionIdentifier(_, morpheus)), selection.getFilePath)
    }

    override def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id] = {
        val ids = getSelectedElements(morpheus, selection).filter {
            case i : Id => true
            case _ => false
        }.asInstanceOf[List[Id]]
        ids.filter(id => CInlineFunction.canInline(morpheus, id))
    }

    private def getFunctionIdentifier(function: AST, morpheus: Morpheus): Id = {
        function match {
            case f: FunctionDef => f.declarator.getId
            case n: NestedFunctionDef => n.declarator.getId
            case c: FunctionCall => getFunctionIdentifier(parentAST(c, morpheus.getASTEnv), morpheus)
            case PostfixExpr(i: Id, _ : FunctionCall) => i
            case _ =>
                logger.warn("No function identifier found for: " + function)
                null
        }
    }

    private def isSelected(element: AST, astEnv: ASTEnv, selection: CodeSelection): Boolean = {
        element match {
            case f: FunctionDef => isPartOfSelection(f.declarator.getId, selection)
            case n: NestedFunctionDef => isPartOfSelection(n.declarator.getId, selection)
            case c: FunctionCall => isSelected(astEnv.parent(c).asInstanceOf[AST], astEnv, selection)
            case p: PostfixExpr => isPartOfSelection(p.p, selection)
            case _ => false
        }
    }
 }
