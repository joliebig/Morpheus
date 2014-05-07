package de.fosd.typechef.crefactor.backend.codeselection

import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.parser.c._
import java.util.Collections
import scala.collection.JavaConversions._
import scala.Some
import de.fosd.typechef.crefactor.backend.RefactorException


object CExtractSelection extends ASTSelection  {

    def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST] = {
        val ids = filterASTElementsForFile[Id](
            filterASTElems[Id](morpheus.getTranslationUnit).par.filter(x => isPartOfSelection(x, selection)).toList, selection.getFilePath)

        // this function tries to find the greatest statement of a selection: for example:
        // if (1) {
        //     i++;
        // }
        // in case the whole if statement is selected we don't want to add the i++ statement to our selection list,
        // as it is already part of the if statement
        def exploitStatement(stmt: Statement): Statement = {
            try {
                parentAST(stmt, morpheus.getASTEnv) match {
                    case null => throw new RefactorException("No proper selection for extract function.")
                    case _: FunctionDef       => stmt
                    case _: NestedFunctionDef => stmt
                    case p =>
                        if (isElementOfSelection(p, selection)) {
                            exploitStatement(p.asInstanceOf[Statement])
                        } else
                            stmt
                }
            } catch {
                case _: Throwable => stmt
            }
        }

        // TODO @ajanker: I don't get the purpose of this function?
        // if an control statement was hit - we look if the afterwards, possible embedded stmts are also selected.
        def lookupControlStatements(stmt: Statement): Statement = {
            nextAST(stmt, morpheus.getASTEnv) match {
                case ns @ ( ContinueStatement() | BreakStatement() | CaseStatement(_) |
                            GotoStatement(_) | ReturnStatement(_)) =>
                    if (isElementOfSelection(ns, selection))
                        ns.asInstanceOf[Statement]
                    else
                        stmt
                case _ => stmt
            }
        }
        val uniqueSelectedStatements = Collections.newSetFromMap[Statement](new java.util.IdentityHashMap())
        val uniqueSelectedExpressions = Collections.newSetFromMap[Expr](new java.util.IdentityHashMap())

        ids.foreach(id => {
            val parent = findPriorASTElem[Statement](id, morpheus.getASTEnv)
            parent match {
                case Some(stmt) =>
                    stmt.setPositionRange(id)
                    uniqueSelectedStatements.add(stmt)
                    uniqueSelectedStatements.add(lookupControlStatements(stmt))
                case None => // logger.info("There may have been an expression!")
            }
        })

        val selectedElements = {
            if (!uniqueSelectedStatements.isEmpty) {
                val parents = uniqueSelectedStatements.toList
                uniqueSelectedStatements.clear()
                parents.foreach(statement => {
                    val exploitedStatement = exploitStatement(statement)
                    uniqueSelectedStatements.add(exploitedStatement)
                })
                uniqueSelectedStatements
            } else
                uniqueSelectedExpressions
        }


        val selected = selectedElements.toList.sortWith(comparePosition)
        logger.info("ExtractFuncSelection: " + selected)
        selected
    }

    def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id] =
        getSelectedElements(morpheus, selection).isEmpty match {
            case true => null
            case false => List[Id]() // returns a empty list to signalize a valid selection was found
        }
}
