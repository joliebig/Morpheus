package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.crefactor._
import de.fosd.typechef.crefactor.backend.{RefactorException, CRefactor, ASTSelection}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.typesystem._
import de.fosd.typechef.conditional._
import scala._
import scala.Some
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.crewrite.IntraCFG
import java.util.Collections
import de.fosd.typechef.parser.c.SwitchStatement
import scala.Some
import de.fosd.typechef.parser.c.NAryExpr
import de.fosd.typechef.parser.c.DoStatement
import de.fosd.typechef.parser.c.Initializer
import de.fosd.typechef.parser.c.AssignExpr
import de.fosd.typechef.parser.c.DeclParameterDeclList
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.DeclarationStatement
import de.fosd.typechef.crefactor.backend.RefactorException
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.parser.c.CaseStatement
import de.fosd.typechef.parser.c.InitDeclaratorE
import de.fosd.typechef.parser.c.PostfixExpr
import de.fosd.typechef.parser.c.ArrayAccess
import de.fosd.typechef.parser.c.ReturnStatement
import de.fosd.typechef.parser.c.CompoundStatementExpr
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.parser.c.FunctionCall
import de.fosd.typechef.parser.c.IfStatement
import de.fosd.typechef.parser.c.NArySubExpr
import de.fosd.typechef.parser.c.WhileStatement
import de.fosd.typechef.parser.c.InitDeclaratorI
import de.fosd.typechef.parser.c.Declaration
import de.fosd.typechef.parser.c.ExprStatement
import de.fosd.typechef.parser.c.ElifStatement
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.NestedFunctionDef
import de.fosd.typechef.parser.c.ParameterDeclarationD
import de.fosd.typechef.parser.c.SwitchStatement
import scala.Some
import de.fosd.typechef.parser.c.NAryExpr
import de.fosd.typechef.parser.c.DoStatement
import de.fosd.typechef.parser.c.Initializer
import de.fosd.typechef.parser.c.AssignExpr
import de.fosd.typechef.conditional.One
import de.fosd.typechef.parser.c.DeclParameterDeclList
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.DeclarationStatement
import de.fosd.typechef.crefactor.backend.RefactorException
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.CaseStatement
import de.fosd.typechef.parser.c.InitDeclaratorE
import de.fosd.typechef.parser.c.PostfixExpr
import de.fosd.typechef.parser.c.ArrayAccess
import de.fosd.typechef.parser.c.ReturnStatement
import de.fosd.typechef.parser.c.CompoundStatementExpr
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.parser.c.FunctionCall
import de.fosd.typechef.parser.c.IfStatement
import de.fosd.typechef.parser.c.NArySubExpr
import de.fosd.typechef.parser.c.WhileStatement
import de.fosd.typechef.parser.c.InitDeclaratorI
import de.fosd.typechef.parser.c.Declaration
import de.fosd.typechef.parser.c.ExprStatement
import de.fosd.typechef.parser.c.ElifStatement
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.NestedFunctionDef
import de.fosd.typechef.parser.c.ParameterDeclarationD

/**
 * Implements inline-function refactoring!
 */
object CInlineFunction extends ASTSelection with CRefactor with IntraCFG {

    def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST] = {
        val functions = (filterASTElems[FunctionDef](morpheus.getTranslationUnit) :::
            filterASTElems[FunctionCall](morpheus.getTranslationUnit) :::
            filterASTElems[NestedFunctionDef](morpheus.getTranslationUnit)).filter(isSelected(_, morpheus.getASTEnv, selection))

        filterASTElementsForFile(functions.map(getFunctionIdentifier(_, morpheus)), selection.getFilePath)
    }

    def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id] = {
        val ids = getSelectedElements(morpheus, selection).filter {
            case i : Id => true
            case _ => false
        }.asInstanceOf[List[Id]]
        ids.filter(id => isAvailable(morpheus, id))
    }

    def isAvailable(morpheus: Morpheus, selection: CodeSelection): Boolean =
        getAvailableIdentifiers(morpheus, selection).nonEmpty

    def isAvailable(morpheus: Morpheus, call: Id): Boolean = {
        if (!isFunctionCall(morpheus, call))
            return false

        val callsDeclDef = divideCallDeclDef(call, morpheus)
        val fDefs = callsDeclDef._3
        val calls = callsDeclDef._1

        if (fDefs.isEmpty) {
            logger.info(call + " is a imported function.")
            false
        } else if (fDefs.exists(func => hasIncompatibleCFG(func.entry, morpheus)
            || isRecursive(func.entry)))  {
            logger.info(call + " is not compatible.")
            false
        } else if (calls.exists(fCall => {
            fDefs.exists(fDef => {
                val callCompStmt = getCallCompStatement(fCall, morpheus.getASTEnv)
                val ids = filterAllASTElems[Id](fDef)
                ids.exists(hasIncompatibleVariableScoping(_, callCompStmt, morpheus))
            })})) {
            logger.info(call + " has different scopes.")
            false
        } else
            true
    }

    def isFunctionCall(morpheus: Morpheus, id: Id): Boolean = {
        parentAST(id, morpheus.getASTEnv) match {
            case PostfixExpr(`id`, FunctionCall(_)) => true
            case _ => false
        }
    }

    /**
     * Perform the actual inlining.
     *
     * @param morpheus the morpheus environment
     * @param id the function's identifier
     * @return the refactored tunit
     */
    def inline(morpheus: Morpheus, id: Id, keepDeclaration : Boolean): Either[String, TranslationUnit] = {
        val (fCalls, fDecls, fDefs, callExpr) = divideCallDeclDef(id, morpheus)

        if (fDefs.isEmpty)
            Left("Inlining of external function definitions is not supported.")

        var tunitRefactored = fCalls.foldLeft(morpheus.getTranslationUnit)((curTunit, curFCall) =>
            inlineFuncCall(curTunit, new Morpheus(curTunit, morpheus.getFM), curFCall, fDefs))

        tunitRefactored =
            callExpr.foldLeft(tunitRefactored)(
                (curTunit, expr) => inlineFuncCallExpr(curTunit,
                    new Morpheus(curTunit, morpheus.getFM), expr, fDefs))

        // Remove old definition and declarations (note may cause linking error)
        if (!keepDeclaration) {
            tunitRefactored = fDefs.foldLeft(tunitRefactored)((workingAST, x) => removeFromAST(workingAST, x))
            tunitRefactored = fDecls.foldLeft(tunitRefactored)((workingAST, x) => removeFromAST(workingAST, x))
        }

        Right(tunitRefactored)
    }

    def divideCallDeclDef(callId: Id, morpheus: Morpheus): (List[Opt[Statement]], List[Opt[AST]],
        List[Opt[FunctionDef]], List[Opt[AST]]) = {
        var fCallStmts = List[Opt[Statement]]()
        var fDecls = List[Opt[AST]]()
        var fDefs = List[Opt[FunctionDef]]()
        var fCallExprs = List[Opt[AST]]()

        morpheus.getReferences(callId).map(_.entry).foreach(id => {
            val parent = parentOpt(id, morpheus.getASTEnv)
            parent.entry match {
                case _: NestedFunctionDef => // not supported
                case _: WhileStatement => fCallExprs ::= parent.asInstanceOf[Opt[AST]]
                case _: Statement => fCallStmts ::= parent.asInstanceOf[Opt[Statement]]
                case _: FunctionDef => fDefs ::= parent.asInstanceOf[Opt[FunctionDef]]
                case iI: InitDeclaratorI =>
                    iI.i match {
                        case None => fDecls ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                        case _ => parentAST(parentAST(id, morpheus.getASTEnv), morpheus.getASTEnv) match {
                            case a: ArrayAccess => logger.warn("hit array funccall " + a + " " + parent)
                            case _ => fCallStmts ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[DeclarationStatement]]
                        }
                    }
                case _: InitDeclaratorE => fDecls ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                case _: Expr => fCallExprs ::= parent.asInstanceOf[Opt[AST]]
                case _: NArySubExpr => fCallExprs ::= parent.asInstanceOf[Opt[AST]]
                case x => logger.warn("Invalid function found!" + x)
            }
        })

        (fCallStmts, fDecls, fDefs, fCallExprs)
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

    /*
     * Retrieves if there are bad conditional return during control flow.
     */
    private def hasIncompatibleCFG(func: FunctionDef, morpheus: Morpheus): Boolean = {

        // this function determines for a given AST node the corresponding statement
        // in the compound statement that is directly attached to the function definition
        def getStatementForAstNode(i: AST): Option[Statement] = {
            findPriorASTElem[FunctionDef](i, morpheus.getASTEnv) match {
                case None => None
                case Some(f) =>
                    f.stmt.innerStatements.foreach {
                        cStmt => if (isPartOf(i, cStmt)) return Some(cStmt.entry)
                    }
                    None
            }
        }

        val fPreds = pred(func, morpheus.getASTEnv).filterNot(x => x.feature isSatisfiable morpheus.getFM)
        val fReturnStmts = fPreds.map(_.entry).filter { case _: ReturnStatement => true; case _ => false }

        if (fReturnStmts.size > 0) {
            val fStmt = getStatementForAstNode(fReturnStmts.head)
            fReturnStmts.tail.forall(isPartOf(_, fStmt))
        } else {
            false
        }
    }

    private def isRecursive(funcDef: FunctionDef): Boolean = {
        filterASTElems[PostfixExpr](funcDef).exists({
            case PostfixExpr(Id(name), FunctionCall(_)) => name.equals(funcDef.getName)
            case _ => false
        })
    }

    private def inlineFuncCallExpr(ast: AST, morpheus: Morpheus, call: Opt[AST],
                                   fDefs: List[Opt[_]]): TranslationUnit = {
        val workingCallCompStmt = getCallCompStatement(call, morpheus.getASTEnv)

        def generateInlineExprStmts: List[(CompoundStatementExpr, FeatureExpr)] = {
            fDefs.flatMap({
                case f: Opt[FunctionDef] =>
                    val inlineExpr = inlineFDefInExpr(workingCallCompStmt, f, call, morpheus)
                    inlineExpr match {
                        case null => None
                        case _ => Some(CompoundStatementExpr(inlineExpr), f.feature.and(call.feature))
                    }
            })
        }

        def inlineInCorrectConditionalStmt(current: Conditional[Expr], call: AST, expr: AST,
                                           inlineChoice: CompoundStatementExpr): Conditional[Expr] = {

            def inlineInOne(entry: Expr, expr: AST, o: One[Expr], call: AST, inlineChoice: CompoundStatementExpr):
            Conditional[Expr] = {
                if (!entry.eq(expr))
                    return o

                expr match {
                    case NAryExpr(e, others) =>
                        call match {
                            case n@NArySubExpr(op, _) =>
                                // TODO: @andreas. statement references the problematic replace function.
                                val replacement = replaceInAST(others, call, n.copy(e = inlineChoice))
                                o.copy(value = NAryExpr(e, replacement.asInstanceOf[List[Opt[NArySubExpr]]]))
                            case x =>
                                assert(false, "Refactoring failed.")
                                o
                        }
                    case p: PostfixExpr => o.copy(value = inlineChoice)
                    case x =>
                        println(x)
                        assert(false, "Refactoring failed.")
                        o
                }
            }

            // TODO: @andreas This code has potential for simplification. I do not fully understand what it does.
            // Simply traversing done to a one and make the appropriate transformation with inlineOne, right?
            // Should be rewritten with a kiama rule.
            current match {
                case o@One(entry) =>
                    inlineInOne(entry, expr, o, call, inlineChoice)
                case c@Choice(_, c1@Choice(_, _, _), c2@Choice(_, _, _)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(c1, call, expr, inlineChoice),
                        elseBranch = inlineInCorrectConditionalStmt(c2, call, expr, inlineChoice))
                case c@Choice(_, o1@One(_), c2@Choice(_, _, _)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(o1, call, expr, inlineChoice),
                        elseBranch = inlineInCorrectConditionalStmt(c2, call, expr, inlineChoice))
                case c@Choice(_, c1@Choice(_, _, _), o2@One(_)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(c1, call, expr, inlineChoice),
                        elseBranch = inlineInCorrectConditionalStmt(o2, call, expr, inlineChoice))
                case c@Choice(_, o1@One(_), o2@One(_)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(o1, call, expr, inlineChoice),
                        elseBranch = inlineInCorrectConditionalStmt(o2, call, expr, inlineChoice))
                case _ => current
            }
        }

        def inlineInExpr(expr: Expr, inlineExprStatements: List[(CompoundStatementExpr, FeatureExpr)]) =
            replaceInAST(expr, call.entry, buildVariableCompoundStatement(inlineExprStatements)).asInstanceOf[Expr]

        findPriorASTElem[Statement](call.entry, morpheus.getASTEnv) match {
            case None =>
                assert(false, "This should not have happend!")
                null
            case Some(entry) =>
                val inlineExprStatements = generateInlineExprStmts
                val parent = parentOpt(entry, morpheus.getASTEnv).asInstanceOf[Opt[Statement]]
                var replaceStmt: CompoundStatement = null
                var callParent = parentAST(call.entry, morpheus.getASTEnv)
                entry match {
                    case i@IfStatement(c@condition, _, elifs, _) =>
                        // TODO Refactor for performance
                        // TODO: @andreas: unclear; What does the code do?
                        if (callParent.eq(i))
                            callParent = call.entry

                        val cond = inlineInCorrectConditionalStmt(condition, call.entry, callParent,
                            buildVariableCompoundStatement(inlineExprStatements))
                        if (!cond.eq(i.condition))
                            replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                                parent.copy(entry = i.copy(condition = cond)))
                        else {
                            val refactoredElifs = elifs.map(elif => {
                                if (parentAST(call.entry, morpheus.getASTEnv).eq(elif.entry))
                                    callParent = call.entry

                                val condition = elif.entry.condition
                                val cond = inlineInCorrectConditionalStmt(condition, call.entry, callParent,
                                    buildVariableCompoundStatement(inlineExprStatements))
                                if (!cond.eq(condition))
                                    elif.copy(entry = elif.entry.copy(condition = cond))
                                else
                                    elif
                            })
                            replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                                parent.copy(entry = i.copy(elifs = refactoredElifs)))
                        }
                    case w: WhileStatement =>
                        replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                            parent.copy(entry = w.copy(expr = inlineInExpr(w.expr, inlineExprStatements))))
                    case s: SwitchStatement =>
                        replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                            parent.copy(entry = s.copy(expr = inlineInExpr(s.expr, inlineExprStatements))))
                    case d: DoStatement =>
                        replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                            parent.copy(entry = d.copy(expr = inlineInExpr(d.expr, inlineExprStatements))))
                    case e: ExprStatement =>
                        replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                            parent.copy(entry = e.copy(expr = inlineInExpr(e.expr, inlineExprStatements))))
                    case c: CaseStatement =>
                        replaceStmt = replaceStmtInCompoundStatement(workingCallCompStmt, parent,
                            parent.copy(entry = c.copy(c = inlineInExpr(c.c, inlineExprStatements))))
                    case x =>
                        logger.error("Missed InlineStatementExpr" + x)
                        throw new RefactorException("Refactoring failed - missed InlineStatementExpr")
                }
                insertRefactoredAST(morpheus, getCallCompStatement(call, morpheus.getASTEnv),
                    replaceStmt)
        }
    }

    private def inlineFuncCall(tunit: TranslationUnit, morpheus: Morpheus, call: Opt[Statement],
                               fDefs: List[Opt[_]]): TranslationUnit = {
        var workingCallCompStmt = getCallCompStatement(call, morpheus.getASTEnv)

        def inlineIfStmt(funcDefs: List[Opt[_]], callCompStmt: CompoundStatement): CompoundStatement = {
            val stmtsToInline = funcDefs.flatMap({
                case f: Opt[FunctionDef] =>
                    val inlineStmt = inlineFDefInExprStmt(callCompStmt, morpheus, call, f)
                    inlineStmt match {
                        case null => None
                        case _ => Some(inlineStmt, f.feature.and(call.feature))
                    }
                case _ =>
                    println("Forgotten definition")
                    None
            })
            // Remove fCall and inline function
            replaceInAST(callCompStmt, morpheus.getASTEnv.parent(call.entry),
                buildChoice(stmtsToInline)).asInstanceOf[CompoundStatement]
        }

        def inlineCompStmt(fDefs: List[Opt[_]], callCompStmt: CompoundStatement): CompoundStatement = {
            val workingCallCompStmt =
                fDefs.foldLeft(callCompStmt)(
                    (curStmt, fDef) => fDef match {
                        case f: Opt[FunctionDef] => inlineFDefInCompStmt(curStmt, morpheus, call, f)
                        case _ =>
                            println("Forgotten definition")
                            curStmt
                    })
            // Remove stmt
            removeFromAST(workingCallCompStmt, call)
        }

        parentAST(call.entry, morpheus.getASTEnv) match {
            case _: CompoundStatement => workingCallCompStmt = inlineCompStmt(fDefs, workingCallCompStmt)
            case _: IfStatement => workingCallCompStmt = inlineIfStmt(fDefs, workingCallCompStmt)
            case _: ElifStatement => workingCallCompStmt = inlineIfStmt(fDefs, workingCallCompStmt)
            case x => println("forgotten " + x)
        }

        insertRefactoredAST(morpheus, getCallCompStatement(call, morpheus.getASTEnv), workingCallCompStmt)
    }

    private def getCallCompStatement(call: Opt[AST], astEnv: ASTEnv): CompoundStatement =
        findPriorASTElem[CompoundStatement](call.entry, astEnv) match {
            case Some(entry) => entry
            case _ => null
        }

    private def assignReturnValue(statement: CompoundStatement, call: Opt[Statement],
                                  returnStmts: List[Opt[ReturnStatement]], morpheus: Morpheus):
    CompoundStatement = {
        // TODO deep inspect
        var wStmt = statement

        def initReturnStatement(decl: Declaration, returnStmt: Opt[ReturnStatement],
                                declStmt: DeclarationStatement, compoundStmt: CompoundStatement,
                                call: Opt[Statement]): CompoundStatement = {
            var initDecls = List[Opt[InitDeclarator]]()
            decl.init.foreach(init => {
                val feature = init.feature.and(returnStmt.feature)
                if (feature.isContradiction(morpheus.getFM))
                    return compoundStmt

                init.entry match {
                    case i@InitDeclaratorI(_, _, Some(Initializer(label, expr))) =>
                        initDecls ::= Opt(feature, i.copy(i = Some(Initializer(label,
                            returnStmt.entry.expr.get))))
                    case x =>
                        println("missed " + x)
                        assert(false, "Pattern matching not exhaustive")
                }
            })
            val declSpecs = decl.declSpecs.map(
                spec => {
                    val feature = spec.feature.and(returnStmt.feature)
                    if (feature.isContradiction(morpheus.getFM))
                        return compoundStmt
                    spec.copy(feature = feature)
                }
            )

            val feature = call.feature.and(returnStmt.feature)
            if (feature.isContradiction(morpheus.getFM))
                return compoundStmt

            val rDecl = decl.copy(declSpecs.reverse, initDecls.reverse)
            // TODO ReplaceStrategy
            replaceInASTOnceTD(compoundStmt.asInstanceOf[AST], returnStmt,
                call.copy(feature, declStmt.copy(rDecl))).asInstanceOf[CompoundStatement]
        }

        def assignStatement(stmt: Opt[Statement], compoundStmt: CompoundStatement,
                            call: Opt[Statement], entry: Expr): CompoundStatement = {
            var wStatement = compoundStmt
            call.entry match {
                case ExprStatement(AssignExpr(t, o, _)) =>
                    val feature = stmt.feature.and(call.feature)
                    if (feature.isSatisfiable(morpheus.getFM))
                        if (wStatement.innerStatements.exists(_.eq(statement)))
                            wStatement = replaceInASTOnceTD(wStatement, stmt,
                                Opt(feature, ExprStatement(AssignExpr(t, o, entry))))
                        else wStatement = replaceInASTOnceTD(wStatement, stmt,
                            Opt(feature, ExprStatement(AssignExpr(t, o, entry))))
                    else wStatement = removeFromAST(wStatement, stmt)
                case _ => assert(false, "Missed Pattern!")
            }
            wStatement
        }

        def includeReturnStatement(returnStmts: List[Opt[Statement]], cStmt: CompoundStatement,
                                   fCall: Opt[Statement], assign: Boolean): CompoundStatement = {
            var wStmt = cStmt
            returnStmts.foreach(stmt => stmt.entry match {
                case ReturnStatement(Some(entry)) =>
                    val feature = stmt.feature.and(fCall.feature)
                    if (assign)
                        wStmt = assignStatement(stmt, wStmt, fCall, entry)
                    else if (feature.isSatisfiable(morpheus.getFM))
                        wStmt = replaceInASTOnceTD(wStmt, stmt, Opt(feature, ExprStatement(entry)))
                case _ => assert(false, "Missed Pattern!")
            })
            wStmt
        }

        call.entry match {
            case ExprStatement(e) => e match {
                case _: PostfixExpr => wStmt = includeReturnStatement(returnStmts, wStmt, call, false)
                case _: AssignExpr => wStmt = includeReturnStatement(returnStmts, wStmt, call, true)
                case x => println("missed" + x)
            }
            case declStmt@DeclarationStatement(decl) =>
                returnStmts.foreach(statement => wStmt = initReturnStatement(decl, statement, declStmt, wStmt, call))
            case x =>
                println("missed " + x)
                assert(false, "Pattern matching not exhaustive")
        }
        wStmt
    }

    private def inlineFDefInCompStmt(compStmt: CompoundStatement, morpheus: Morpheus, fCall: Opt[Statement],
                                     fDef: Opt[FunctionDef]): CompoundStatement = {
        if (!isValidFDef(fDef, fCall, compStmt, morpheus))
            return compStmt

        var workingStatement = compStmt
        val idsToRename = getIdsToRename(fDef.entry, workingStatement, morpheus)
        val renamed = renameShadowedIds(idsToRename, fDef, fCall, morpheus)
        val initializer = getInitializers(fCall, renamed._2, morpheus)

        // apply feature environment
        val statements = applyFeaturesOnInlineStmts(renamed._1, fCall, morpheus)

        // find return statements
        val returnStmts = getReturnStmts(statements)

        // insert in tunit
        workingStatement = insertInAstBefore(workingStatement, fCall, initializer)
        workingStatement = insertInAstBefore(workingStatement, fCall, statements)

        // insert return statements
        assignReturnValue(workingStatement, fCall, returnStmts, morpheus)
    }

    private def inlineFDefInExprStmt(compStmt: CompoundStatement, morpheus: Morpheus, fCall: Opt[AST],
                                     fDef: Opt[FunctionDef]): ExprStatement = {
        if (!isValidFDef(fDef, fCall, compStmt, morpheus))
            return null

        val compoundStmtExpr: CompoundStatementExpr =
            CompoundStatementExpr(inlineFDefInExpr(compStmt, fDef, fCall, morpheus))

        fCall.entry match {
            case ExprStatement(PostfixExpr(_, FunctionCall(_))) => ExprStatement(compoundStmtExpr)
            case ExprStatement(AssignExpr(target, op, _)) => ExprStatement(AssignExpr(target, op, compoundStmtExpr))
            case _ =>
                assert(false, "An error occurred. Unable to assign return statements")
                null
        }
    }


    private def inlineFDefInExpr(compStmt: CompoundStatement, fDef: Opt[FunctionDef],
                                 fCall: Opt[AST], morpheus: Morpheus): CompoundStatement = {
        val workingStatement = compStmt

        val idsToRename = getIdsToRename(fDef.entry, workingStatement, morpheus)

        val renamed = renameShadowedIds(idsToRename, fDef, fCall, morpheus)
        val initializer = getInitializers(fCall, renamed._2, morpheus)
        var stmts = applyFeaturesOnInlineStmts(renamed._1, fCall, morpheus)
        val returnStmts = getReturnStmts(stmts)

        // remove return statements
        stmts = returnStmts.foldLeft(stmts)((stmts, returnStmt) =>
            returnStmt.entry.expr match {
                case None => removeFromAST(stmts, returnStmt)
                case Some(_) => replaceInAST(stmts, returnStmt,
                    Opt(returnStmt.feature, ExprStatement(returnStmt.entry.expr.get)))
            })
        CompoundStatement(initializer ::: stmts)
    }

    /**
     * Determines if a function has an identifier which has a different scope under different presence condition.
     * As renaming of globally scoped variables causes to alter the behaviour, we do not allow inining of functions
     * containing variables which are not in the same scope under each condition.
     */
    private def hasIncompatibleVariableScoping(id: Id, compStmt: CompoundStatement, morpheus: Morpheus): Boolean = {
        val env = morpheus.getEnv(compStmt.innerStatements.last.entry)
        val condScope = env.varEnv.lookupScope(id.name)
        val variableScope = -1

        def checkConditional(conditional: Conditional[Int]): Boolean = {
            def checkScopes(condScope: Conditional[Int]): Int = {
                condScope match {
                    case Choice(_, thenB, elseB) =>
                        val thenScope = checkScopes(thenB)
                        val elseScope = checkScopes(elseB)

                        if (thenScope == variableScope || elseScope == variableScope) variableScope
                        else if (thenScope != elseScope) variableScope
                        else thenScope
                    case One(scope) => scope
                }
            }
            
            checkScopes(conditional) == variableScope
        }

        condScope match {
            case One(scope) => false
            case _ => checkConditional(condScope)
        }
    }

    private def isValidFDef(fDef: Opt[FunctionDef], call: Opt[_], ccStatement: CompoundStatement,
                            morpheus: Morpheus): Boolean = {
        if (!(fDef.feature.equivalentTo(FeatureExprFactory.True)
            || fDef.feature.implies(call.feature).isTautology(morpheus.getFM)))
            return false

        // stmt's feature does not imply fDef's feature -> no need to inline this def at this position
        if (isRecursive(fDef.entry))
            throw new RefactorException("Can not inline - method is recursive.")
        if (hasIncompatibleCFG(fDef.entry, morpheus))
            throw new RefactorException("Can not inline - method has bad return statements")

        true
    }

    private def getIdsToRename(funcDef: FunctionDef, compStmt: CompoundStatement, morpheus: Morpheus) = {
        val idsToInline = filterAllASTElems[Id](funcDef)
        idsToInline.filter(isDeclaredInFunctionCallScope(_, compStmt, morpheus))
    }

    private def isFunctionDefOrDecl(id: Id, morpheus: Morpheus): Boolean = {
        if (morpheus.getEnv(id).varEnv.lookupType(id.name).forall(_.isFunction))
            return true

        morpheus.getReferences(id).map(_.entry).exists(ref => {
            findPriorASTElem[FunctionDef](ref, morpheus.getASTEnv) match {
                case Some(f) => f.declarator.getName.equals(ref.name)
                case _ => false
            }
        })
    }

    private def isPartOfStruct(refs: List[Opt[Id]], morpheus: Morpheus): Boolean = {
        refs.map(_.entry).exists(ref => {
            findPriorASTElem[StructDeclarator](ref, morpheus.getASTEnv) match {
                case Some(_) => true
                case _ => false
            }
        })
    }

    private def isTypeDef(refs: List[Opt[Id]], morpheus: Morpheus): Boolean = {
        refs.map(_.entry).exists(ref => {
            findPriorASTElem[Declaration](ref, morpheus.getASTEnv) match {
                case Some(decl) => {
                    decl.init.exists(_.entry.getName.equals(ref.name)) && decl.declSpecs.exists(_.entry match {
                        case _: TypedefSpecifier => true
                        case _ => false
                    })
                }
                case _ => false
            }
        })
    }

    /**
     * Checks if an id is only declared at the place of the inlining function scope.
     */
    private def isDeclaredInFunctionCallScope(id: Id, callCompStmt: CompoundStatement, morpheus: Morpheus): Boolean = {

        // lookup if name is visible in current scope
        if (!isVisibleNameInFunctionCallScope(id.name, callCompStmt, morpheus))
            return false

        // check if any reference of the inline id links into the calling scope - if so no renaming is required
        val idReferences = morpheus.getReferences(id)
        val idsInCompStmt = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        filterAllASTElems[Id](callCompStmt) foreach idsInCompStmt.add

        if (idReferences.exists(idsInCompStmt.contains))
            return false

        // id is function -> no rename
        if (isFunctionDefOrDecl(id, morpheus))
            return false

        // id is part of struct -> no rename
        if (isPartOfStruct(idReferences, morpheus))
            return false

        // id is typedef -> no rename
        if (isTypeDef(idReferences, morpheus))
            return false

        true
    }

    /**
     * Checks if a name is visibile at the place of the inlining function scope.
     */
    private def isVisibleGlobalNameInFunctionCallScope(name: String, callCompStmt: CompoundStatement, morpheus: Morpheus) =
        isPartOfScope(name, callCompStmt, morpheus, 0)

    /**
     * Checks if a name is visibile at the place of the inlining function scope.
     */
    private def isVisibleNameInFunctionCallScope(name: String, callCompStmt: CompoundStatement, morpheus: Morpheus) =
        !isPartOfScope(name, callCompStmt, morpheus, -1)

    /**
     * Checks if a name is part of a specific scope.
     */
    private def isPartOfScope(name: String, compStmt: CompoundStatement, morpheus : Morpheus, scope : Int) : Boolean = {
        val env = morpheus.getEnv(compStmt.innerStatements.last.entry)
        val nameScope = env.varEnv.lookupScope(name)

        def checkOne(one: Conditional[Int]): Boolean = {
            one match {
                case One(`scope`) => true
                case _ => false
            }
        }

        def checkConditionalScope(conditional: Conditional[Int]): Boolean = {
            conditional match {
                case Choice(_, thenB, elseB) =>
                    checkConditionalScope(thenB) || checkConditionalScope(elseB)
                case One((_)) => checkOne(conditional)
            }
        }

        checkConditionalScope(nameScope)
    }

    private def getInitializers(call: Opt[AST], parameters: List[Opt[DeclaratorExtension]],
                                morpheus: Morpheus): List[Opt[DeclarationStatement]] = {

        def generateInitializer(parameter: Opt[DeclaratorExtension], exprList: List[Opt[Expr]]):
        List[Opt[DeclarationStatement]] = {
            var exprs = exprList
            parameter.entry match {
                case p: DeclParameterDeclList =>
                    p.parameterDecls.flatMap(pDecl => {
                        val expr = exprs.head
                        val feature = expr.feature.and(parameter.feature)

                        if (!feature.isSatisfiable(morpheus.getFM))
                            None
                        else {
                            exprs = exprs.tail
                            pDecl.entry match {
                                case p: ParameterDeclarationD =>
                                    val spec = p.specifiers.map(specifier => specifier.copy(feature = feature.and(specifier.feature)))
                                    Some(Opt(feature, DeclarationStatement(Declaration(spec, List(Opt(feature, InitDeclaratorI(p.decl, List(), Some(Initializer(None, expr.entry)))))))))
                                case _ =>
                                    assert(false, "Can not init parameters!")
                                    None
                            }
                        }
                    })
                case _ =>
                    assert(false, "Can not init parameters!")
                    null
            }
        }
        // TODO Safe solution -> features
        val exprList = filterASTElems[FunctionCall](call).head.params.exprs
        parameters.flatMap(parameter => {
            if (exprList.isEmpty) None
            else generateInitializer(parameter, exprList) match {
                case null => None
                case x => Some(x)
            }
        }).foldLeft(List[Opt[DeclarationStatement]]())((result, entry) => result ::: entry)
    }

    private def renameShadowedIds(idsToRename: List[Id], fDef: Opt[FunctionDef], fCall: Opt[AST],
                                  morpheus: Morpheus): (List[Opt[Statement]],
        List[Opt[DeclaratorExtension]]) = {
        val statements = idsToRename.foldLeft(fDef.entry.stmt.innerStatements)((statement, id) => replaceInAST(statement, id, id.copy(name = generateValidNewName(id, fCall, morpheus))))
        val parameters = idsToRename.foldLeft(fDef.entry.declarator.extensions)((extension, id) => replaceInAST(extension, id, id.copy(name = generateValidNewName(id, fCall, morpheus))))
        (statements, parameters)
    }

    private def applyFeaturesOnInlineStmts(statements: List[Opt[Statement]], call: Opt[AST],
                                           morpheus: Morpheus): List[Opt[Statement]] = {
        statements.flatMap(statement => {
            val feature = statement.feature.and(call.feature)
            feature.isSatisfiable(morpheus.getFM) match {
                case true => Some(statement.copy(feature = feature))
                case _ => None
            }
        })
    }

    private def getReturnStmts(statements: List[Opt[Statement]]): List[Opt[ReturnStatement]] = {
        filterAllOptElems(statements).filter {
            case Opt(_, _: ReturnStatement) => true
            case _ => false
        }.asInstanceOf[List[Opt[ReturnStatement]]]
    }
}