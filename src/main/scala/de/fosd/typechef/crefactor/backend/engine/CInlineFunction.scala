package de.fosd.typechef.crefactor.backend.engine

import java.util.Collections

import de.fosd.typechef.conditional._
import de.fosd.typechef.crefactor._
import de.fosd.typechef.crefactor.backend.{RefactorException, CRefactor}
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.parser.c._


/**
 * Implements inline-function refactoring!
 * We support inlining of function calls as part of expressions (e.g., x + foo();) and
 * single statements (e.g., foo();).
 */
object CInlineFunction extends CRefactor with IntraCFG {

    // TODO Integrate in inline
    def canInline(morpheus: Morpheus, fCall: Id): Boolean = {
        if (!isFunctionCall(morpheus, fCall))
            return false

        val (fCalls, _, fDefs, _) = getCallDeclDefCallExprs(fCall, morpheus)

        if (fDefs.isEmpty) {
            logger.info(fCall + " is a imported function.")
            false
        } else if (fDefs.exists(func => hasIncompatibleCFG(func.entry, morpheus)
            || isRecursive(func.entry)))  {
            logger.info(fCall + " is not compatible.")
            false
        } else if (fCalls.exists(fc => {
            fDefs.exists(fd => {
                val callCompStmt = getCompStatement(fc, morpheus.getASTEnv)
                val ids = filterAllASTElems[Id](fd)
                ids.exists(hasIncompatibleVariableScoping(_, callCompStmt, morpheus))
            })})) {
            logger.info(fCall + " has different scopes.")
            false
        } else
            true
    }

    /**
     * Perform the actual inlining.
     *
     * @param morpheus the morpheus environment
     * @param id the function's identifier
     * @return error string (left) or the refactored tunit (right)
     */
    def inline(morpheus: Morpheus, id: Id, keepDeclaration : Boolean): Either[String, TranslationUnit] = {
        val (fCalls, fDecls, fDefs, callExpr) = getCallDeclDefCallExprs(id, morpheus)

        if (fDefs.isEmpty)
            Left("Inlining of external function definitions is not supported.")

        try {
            var tunitRefactored = fCalls.foldLeft(morpheus.getTranslationUnit)((curTunit, curFCall) =>
                inlineFuncCall(curTunit, new Morpheus(curTunit, morpheus.getFM), curFCall, fDefs))

            tunitRefactored =
                callExpr.foldLeft(tunitRefactored)(
                    (curTunit, expr) => inlineFuncCallExpr(curTunit,
                        new Morpheus(curTunit, morpheus.getFM), expr, fDefs))

            // Remove old definition and declarations (note may cause linking error)
            if (!keepDeclaration) {
                tunitRefactored = fDefs.foldLeft(tunitRefactored)((workingAST, x) => remove(workingAST, x))
                tunitRefactored = fDecls.foldLeft(tunitRefactored)((workingAST, x) => remove(workingAST, x))
            }

            Right(tunitRefactored)
        } catch {
            case e: Exception =>
                e.printStackTrace()
                Left(e.getMessage)
        }
    }

    // Starting from a function-call identifier (fCall), we explore reference information to get
    // function-call statements (fCallStmts), function declarations (fDecls), function definitions (fDefs),
    // and function-call expressions (fCallExprs; function calls somewhere inside a statement or expression).
    def getCallDeclDefCallExprs(fCall: Id, morpheus: Morpheus): (List[Opt[Statement]], List[Opt[AST]],
        List[Opt[FunctionDef]], List[Opt[AST]]) = {
        var fCallStmts = List[Opt[Statement]]()
        var fDecls = List[Opt[AST]]()
        var fDefs = List[Opt[FunctionDef]]()
        var fCallExprs = List[Opt[AST]]()

        morpheus.getReferences(fCall).map(_.entry).foreach(id => {
            val parent = parentOpt(id, morpheus.getASTEnv)
            parent.entry match {
                case _: NestedFunctionDef => // not supported
                case _: WhileStatement => fCallExprs ::= parent.asInstanceOf[Opt[AST]]
                case _: Statement => fCallStmts ::= parent.asInstanceOf[Opt[Statement]]
                case _: FunctionDef => fDefs ::= parent.asInstanceOf[Opt[FunctionDef]]
                case InitDeclaratorI(_, _, None) => fDecls ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                case InitDeclaratorI(_, _, Some(_)) =>
                    parentAST(parentAST(id, morpheus.getASTEnv), morpheus.getASTEnv) match {
                        case a: ArrayAccess => logger.warn("hit array funccall " + a + " " + parent)
                        case _ => fCallStmts ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[DeclarationStatement]]
                    }
                case _: InitDeclaratorE => fDecls ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                case _: Expr => fCallExprs ::= parent.asInstanceOf[Opt[AST]]
                case _: NArySubExpr => fCallExprs ::= parent.asInstanceOf[Opt[AST]]
                case x => logger.warn("Invalid function found!" + x)
            }
        })

        (fCallStmts, fDecls, fDefs, fCallExprs)
    }

    /*
     * We do not support the integration of functions with multiple return statements because integrating their
     * control flow on the call site is complex.
     *
     * We check the function definition (fDef) for return expressions that belong to different return statements.
     * If fDef has varying return statements, it is incompatible, otherwise fDef is compatible.
     *
     * Example:
     * int foo() { ... return x; }                              // is compatible
     * int foo() { if (...) { return x+1;} else { return x+2; } // is not compatible
     */
    private def hasIncompatibleCFG(fDef: FunctionDef, morpheus: Morpheus): Boolean = {

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

        val fPreds = pred(fDef, morpheus.getASTEnv).filterNot(x => x.feature isSatisfiable morpheus.getFM)
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
            case PostfixExpr(Id(name), FunctionCall(_)) => name == funcDef.getName
            case _ => false
        })
    }

    private def inlineFuncCallExpr(ast: AST, morpheus: Morpheus, call: Opt[AST],
                                   fDefs: List[Opt[_]]): TranslationUnit = {
        val workingCallCompStmt = getCompStatement(call, morpheus.getASTEnv)

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
                                val replacement = replaceNArySubExpr(others, n, n.copy(e = inlineChoice))
                                o.copy(value = NAryExpr(e, replacement))
                            case x =>
                                logger.warn("missed " + x)
                                throw new RefactorException("No rule defined for:" + x)
                        }
                    case p: PostfixExpr => o.copy(value = inlineChoice)
                    case x =>
                        logger.warn("missed " + x)
                        throw new RefactorException("No rule defined for:" + x)
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

        def inlineInExpr(expr: Expr, inlineExprStatements: List[(CompoundStatementExpr, FeatureExpr)]) = {
            call.entry match {
                case e: Expr =>
                    replaceExprWithCompStmExpr(expr, e, buildVariableCompoundStatement(inlineExprStatements))
                case x => throw new RefactorException("Function call is no expression:" + x)
            }

        }

        findPriorASTElem[Statement](call.entry, morpheus.getASTEnv) match {
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
                insertRefactoredAST(morpheus, getCompStatement(call, morpheus.getASTEnv), replaceStmt)
            case _ => throw new RefactorException("FunctionCall to inline is no statement.")
        }
    }

    private def inlineFuncCall(tunit: TranslationUnit, morpheus: Morpheus, call: Opt[Statement],
                               fDefs: List[Opt[_]]): TranslationUnit = {
        var workingCallCompStmt = getCompStatement(call, morpheus.getASTEnv)

        def inlineIfStmt(funcDefs: List[Opt[_]], callCompStmt: CompoundStatement): CompoundStatement = {
            val stmtsToInline = funcDefs.flatMap({
                case f: Opt[FunctionDef] =>
                    val inlineStmt = inlineFDefInExprStmt(callCompStmt, morpheus, call, f)
                    inlineStmt match {
                        case null => None
                        case _ => Some(inlineStmt, f.feature.and(call.feature))
                    }
                case x =>
                    logger.error("Forgotten definition: " + x)
                    None
            })
            // Remove fCall and inline function
            replaceStmtWithStmtsInCompStmt(callCompStmt, call,
                stmtsToInline.map(toInline => Opt(toInline._2, toInline._1)))
        }

        def inlineCompStmt(fDefs: List[Opt[_]], callCompStmt: CompoundStatement): CompoundStatement = {
            val workingCallCompStmt =
                fDefs.foldLeft(callCompStmt)(
                    (curStmt, fDef) => fDef match {
                        case f: Opt[FunctionDef] => inlineFDefInCompStmt(curStmt, morpheus, call, f)
                        case x =>
                            logger.error("Forgotten definition" + x)
                            curStmt
                    })
            // Remove stmt
            remove(workingCallCompStmt, call)
        }

        parentAST(call.entry, morpheus.getASTEnv) match {
            case _: CompoundStatement => workingCallCompStmt = inlineCompStmt(fDefs, workingCallCompStmt)
            case _: IfStatement => workingCallCompStmt = inlineIfStmt(fDefs, workingCallCompStmt)
            case _: ElifStatement => workingCallCompStmt = inlineIfStmt(fDefs, workingCallCompStmt)
            case x => logger.warn("forgotten " + x)
        }

        insertRefactoredAST(morpheus, getCompStatement(call, morpheus.getASTEnv), workingCallCompStmt)
    }

    private def getCompStatement(call: Opt[AST], astEnv: ASTEnv): CompoundStatement =
        findPriorASTElem[CompoundStatement](call.entry, astEnv) match {
            case Some(entry) => entry
            case _ => null
        }

    private def assignReturnValue(callCompStmt: CompoundStatement, fCall: Opt[Statement],
                                  returnStmts: List[Opt[ReturnStatement]], morpheus: Morpheus): CompoundStatement = {
        var wStmt = callCompStmt

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
                        logger.warn("missed " + x)
                        throw new RefactorException("No rule defined for:" + x)
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
            replaceOnceTD(compoundStmt.asInstanceOf[AST], returnStmt,
                call.copy(feature, declStmt.copy(rDecl))).asInstanceOf[CompoundStatement]
        }

        def assignStatement(stmt: Opt[Statement], compoundStmt: CompoundStatement,
                            call: Opt[Statement], entry: Expr): CompoundStatement = {
            var wStatement = compoundStmt
            call.entry match {
                case ExprStatement(AssignExpr(t, o, _)) =>
                    val feature = stmt.feature.and(call.feature)
                    if (feature.isSatisfiable(morpheus.getFM))
                        if (wStatement.innerStatements.exists(_.eq(callCompStmt)))
                            wStatement = replaceOnceTD(wStatement, stmt,
                                Opt(feature, ExprStatement(AssignExpr(t, o, entry))))
                        else wStatement = replaceOnceTD(wStatement, stmt,
                            Opt(feature, ExprStatement(AssignExpr(t, o, entry))))
                    else wStatement = remove(wStatement, stmt)
                case x =>
                    logger.error("No assign rule for: " + x)
                    throw new RefactorException("No rule defined for:" + x)
            }
            wStatement
        }

        def includeReturnStatement(returnStmts: List[Opt[ReturnStatement]], cStmt: CompoundStatement,
                                   fCall: Opt[Statement], assign: Boolean): CompoundStatement = {
            var wStmt = cStmt
            returnStmts.foreach(stmt => {
                if (stmt.entry.expr.nonEmpty) {
                    val returnStmtExpr = stmt.entry.expr.get
                    val feature = stmt.feature.and(fCall.feature)
                    if (assign)
                        wStmt = assignStatement(stmt, wStmt, fCall, returnStmtExpr)
                    else if (feature.isSatisfiable(morpheus.getFM))
                        wStmt = replaceOnceTD(wStmt, stmt, Opt(feature, ExprStatement(returnStmtExpr)))
                }
            })
            wStmt
        }

        fCall.entry match {
            // function has been called as an expression:
            // a = functionToInline();
            // or
            // functionToInline
            case ExprStatement(e) => e match {
                case _: PostfixExpr => wStmt = includeReturnStatement(returnStmts, wStmt, fCall, false)
                case _: AssignExpr => wStmt = includeReturnStatement(returnStmts, wStmt, fCall, true)
                case x => logger.warn("missed" + x)
            }
            // function to inline has been called as declaration, e.g int foo = functionToInline();
            case declStmt@DeclarationStatement(decl) =>
                returnStmts.foreach(statement => wStmt = initReturnStatement(decl, statement, declStmt, wStmt, fCall))
            // function to inline has been called like return functionToInline()
            case ReturnStatement(expr) =>
            // no initializing or special handling required;
            case x =>
                logger.warn("Pattern not reached " + x)
                throw new RefactorException("Could not inline - some rules are not complete.")
        }
        wStmt
    }

    private def inlineFDefInCompStmt(compStmt: CompoundStatement, morpheus: Morpheus, fCall: Opt[Statement],
                                     fDef: Opt[FunctionDef]): CompoundStatement = {
        if (!isValidFDef(fDef, fCall, morpheus))
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
        workingStatement = insertListBefore(workingStatement, fCall, initializer)
        workingStatement = insertListBefore(workingStatement, fCall, statements)

        // insert return statements
        assignReturnValue(workingStatement, fCall, returnStmts, morpheus)
    }

    private def inlineFDefInExprStmt(compStmt: CompoundStatement, morpheus: Morpheus, fCall: Opt[AST],
                                     fDef: Opt[FunctionDef]): ExprStatement = {
        if (!isValidFDef(fDef, fCall, morpheus))
            return null

        val compoundStmtExpr: CompoundStatementExpr =
            CompoundStatementExpr(inlineFDefInExpr(compStmt, fDef, fCall, morpheus))

        fCall.entry match {
            case ExprStatement(PostfixExpr(_, FunctionCall(_))) => ExprStatement(compoundStmtExpr)
            case ExprStatement(AssignExpr(target, op, _)) => ExprStatement(AssignExpr(target, op, compoundStmtExpr))
            case x =>
                logger.warn("missed " + x)
                throw new RefactorException("No rule defined for assigning for following return statement:" + x)
        }
    }


    private def inlineFDefInExpr(compStmt: CompoundStatement, fDef: Opt[FunctionDef],
                                 fCall: Opt[AST], morpheus: Morpheus): CompoundStatement = {
        val workingStatement = compStmt

        val idsToRename = getIdsToRename(fDef.entry, workingStatement, morpheus)

        val (renamedIdsStmts, renamedIdsParams) = renameShadowedIds(idsToRename, fDef, fCall, morpheus)
        val initializer = getInitializers(fCall, renamedIdsParams, morpheus)
        var stmts = applyFeaturesOnInlineStmts(renamedIdsStmts, fCall, morpheus)
        val returnStmts = getReturnStmts(stmts)

        // replace (return expr; => expr;) or remove (return; => <removed>) return statements
        stmts = returnStmts.foldLeft(stmts)((stmts, returnStmt) =>
            returnStmt.entry.expr match {
                case None => remove(stmts, returnStmt)
                case Some(_) => replace(stmts, returnStmt,
                    Opt(returnStmt.feature, ExprStatement(returnStmt.entry.expr.get)))
            })
        CompoundStatement(initializer ::: stmts)
    }

    /**
     * Determines if the identifier id occurs in different scopes.
     * TODO ajanker: The following comment is unclear. Please describe the problem in more detail!
     * As renaming of globally scoped variables causes to alter the behaviour, we do not allow inlining of functions
     * containing identifiers of variables which are not in the same scope under each condition.
     */
    private def hasIncompatibleVariableScoping(id: Id, compStmt: CompoundStatement, morpheus: Morpheus): Boolean = {
        val env = morpheus.getEnv(compStmt.innerStatements.last.entry)
        val condScope = env.varEnv.lookupScope(id.name)
        val variableScope = -1

        // TODO ajanker: Could be rewritten with:
        // ConditionalLib.leaves(condScope).forall(cs => cs == variableScope)

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

    private def isValidFDef(fDef: Opt[FunctionDef], fCall: Opt[_], morpheus: Morpheus): Boolean = {
        // If a function call's feature does not imply a function definition's feature,
        // then there is no need to inline this definition at this call.
        // TODO ajanker: The comment does not reflect the following condition.
        // fcall.feature.implies(fDef.feature).isTautology(morpheus.getFM)
        // Furthermore, the isTautology seems overly strict because function definition and function
        // call have to have the same (i.e., equivalent) annotation.
        // fcall.feature.implies(fDef.feature).isSatisfiable(morpheus.getFM) should do it.
        // I suggest You check it with an example.
        if (!(fDef.feature.equivalentTo(FeatureExprFactory.True)
            || fDef.feature.implies(fCall.feature).isTautology(morpheus.getFM)))
            return false

        if (isRecursive(fDef.entry))
            throw new RefactorException("Can not inline - method is recursive.")
        if (hasIncompatibleCFG(fDef.entry, morpheus))
            throw new RefactorException("Can not inline - method has bad return statements")

        true
    }

    private def getIdsToRename(fDEf: FunctionDef, compStmt: CompoundStatement, morpheus: Morpheus) = {
        val idsToInline = filterAllASTElems[Id](fDEf)
        idsToInline.filter(isDeclaredInFunctionCallScope(_, compStmt, morpheus))
    }

    private def isFDefOrFDecl(id: Id, morpheus: Morpheus): Boolean = {
        try {
            if (morpheus.getEnv(id).varEnv.lookupType(id.name).forall(_.isFunction))
                return true
        } catch {
            case e: Exception =>
        }

        morpheus.getReferences(id).map(_.entry).exists(ref => {
            findPriorASTElem[FunctionDef](ref, morpheus.getASTEnv) match {
                case Some(f) => f.declarator.getName == ref.name
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
                case Some(decl) =>
                    decl.init.exists(_.entry.getName == ref.name) && decl.declSpecs.exists(_.entry match {
                        case _: TypedefSpecifier => true
                        case _ => false
                    })
                case _ => false
            }
        })
    }

    /**
     * Checks if an id is only declared at the place of the inlining function scope.
     */
    private def isDeclaredInFunctionCallScope(id: Id, callCompStmt: CompoundStatement, morpheus: Morpheus): Boolean = {

        // lookup if name is visible in current scope
        if (!isVisibleNameInFunctionScope(id.name, callCompStmt, morpheus))
            return false

        // check if any reference of the inline id links into the calling scope - if so no renaming is required
        val idReferences = morpheus.getReferences(id)
        val idsInCompStmt = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        filterAllASTElems[Id](callCompStmt) foreach idsInCompStmt.add

        if (idReferences.exists(idsInCompStmt.contains))
            return false

        // in both places the has the same global visibility -> no need to rename
        val localCompStmt = getCompStatement(parentOpt(id, morpheus.getASTEnv).asInstanceOf[Opt[AST]], morpheus.getASTEnv)
        if((localCompStmt != null) && isVisibleGlobalNameInFunctionScope(id.name, callCompStmt, morpheus)
            && isVisibleGlobalNameInFunctionScope(id.name, localCompStmt, morpheus))
            return false

        // id is function -> no rename
        if (isFDefOrFDecl(id, morpheus))
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
     * Checks if a symbol is visible at the place of the inlining function scope.
     */
    private def isVisibleGlobalNameInFunctionScope(name: String, callCompStmt: CompoundStatement, morpheus: Morpheus) =
        isPartOfScope(name, callCompStmt, morpheus, 0)

    /**
     * Checks if a symbol is visible at the place of the inlining function scope.
     */
    private def isVisibleNameInFunctionScope(symbol: String, callCompStmt: CompoundStatement, morpheus: Morpheus) =
        !isPartOfScope(symbol, callCompStmt, morpheus, -1)

    /**
     * Checks if a symbol is part of a specific scope.
     */
    private def isPartOfScope(symbol: String, compStmt: CompoundStatement, morpheus: Morpheus, scope: Int): Boolean = {
        val env = morpheus.getEnv(compStmt.innerStatements.last.entry)
        val nameScope = env.varEnv.lookupScope(symbol)

        // TODO ajanker: Is there a reason why we traverse nameScope without a feature expression?
        ConditionalLib.leaves(nameScope).exists(s => s == scope)
    }

    private def getInitializers(call: Opt[AST], params: List[Opt[DeclaratorExtension]],
                                morpheus: Morpheus): List[Opt[DeclarationStatement]] = {

        def generateInitializer(param: Opt[DeclaratorExtension], exprList: List[Opt[Expr]]):
        List[Opt[DeclarationStatement]] = {
            var exprs = exprList
            param.entry match {
                case p: DeclParameterDeclList =>
                    p.parameterDecls.flatMap(pDecl => {
                        val expr = exprs.head
                        val feature = expr.feature.and(param.feature)

                        if (!feature.isSatisfiable(morpheus.getFM))
                            None
                        else {
                            exprs = exprs.tail
                            pDecl.entry match {
                                case p: ParameterDeclarationD =>
                                    val spec = p.specifiers.map(s => s.copy(feature = feature.and(s.feature)))
                                    Some(Opt(feature,
                                        DeclarationStatement(
                                            Declaration(spec, List(Opt(feature,
                                                InitDeclaratorI(p.decl, List(), Some(Initializer(None, expr.entry)))))))))
                                case x =>
                                    logger.warn("missed " + x)
                                    throw new RefactorException("No rule defined for initializing parameter:" + x)
                            }
                        }
                    })
                case x =>
                    logger.warn("missed " + x)
                    throw new RefactorException("No rule defined for initializing parameter list:" + x)
            }
        }
        // TODO Safe solution -> features
        val exprList = filterASTElems[FunctionCall](call).head.params.exprs
        params.flatMap(parameter => {
            if (exprList.isEmpty) None
            else generateInitializer(parameter, exprList) match {
                case null => None
                case x => Some(x)
            }
        }).flatten
    }

    private def renameShadowedIds(idsToRename: List[Id], fDef: Opt[FunctionDef], fCall: Opt[AST],
                                  morpheus: Morpheus): (List[Opt[Statement]], List[Opt[DeclaratorExtension]]) = {
        val statements = idsToRename.foldLeft(fDef.entry.stmt.innerStatements)(
            (statement, id) => replace(statement, id, id.copy(name = generateValidNewName(id, fCall, morpheus))))
        val parameters = idsToRename.foldLeft(fDef.entry.declarator.extensions)(
            (extension, id) => replace(extension, id, id.copy(name = generateValidNewName(id, fCall, morpheus))))
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

    private def isFunctionCall(morpheus: Morpheus, id: Id): Boolean = {
        parentAST(id, morpheus.getASTEnv) match {
            case PostfixExpr(`id`, FunctionCall(_)) => true
            case _ => false
        }
    }
}