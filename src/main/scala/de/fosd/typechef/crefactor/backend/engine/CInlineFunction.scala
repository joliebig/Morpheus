package de.fosd.typechef.crefactor.backend.engine

import java.util.Collections


import de.fosd.typechef.conditional._
import de.fosd.typechef.crefactor._
import de.fosd.typechef.crefactor.backend.engine.CExtractFunction._
import de.fosd.typechef.crefactor.backend.{RefactorException, CRefactor}
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.parser.c._


/**
 * Implements inline-function refactoring!
 * We support inlining of function calls as part of expressions (e.g., x + foo();) and
 * single statements (e.g., foo();).
 */
object CInlineFunction extends CRefactor with IntraCFG {

    def canInline(morpheus: Morpheus, fCall: Id): Boolean = {

        if (!isFunctionCall(morpheus, fCall))
            return false

        val (fCalls, _, fDefs, _) = getCallDeclDefCallExprs(fCall, morpheus)

        if (fDefs.isEmpty) {
            // function definitons must be available
            // logger.info(fCall + " is a imported function.")
            false
        } else if (fDefs.exists(func => hasIncompatibleCFG(func.entry, morpheus)
            || isRecursive(func.entry) || hasVArgsParameters(func.entry))) {
            // function has  multiple return statements or is recursive or has an variable amount of arguments
            // logger.info(fCall + " is not compatible.")
            false
        } else if (fCalls.exists(fc => {
            fDefs.exists(fd => {
                val callCompStmt = getCompStatement(fc._2, morpheus.getASTEnv)
                val ids = filterAllASTElems[Id](fd)
                ids.exists(hasIncompatibleVariableScoping(_, callCompStmt, morpheus))
            })
        })) {
            // inlined variables does not differ in scoping
            // logger.info(fCall + " has different scopes.")
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
    def inline(morpheus: Morpheus, id: Id, keepDeclaration: Boolean): Either[String, TranslationUnit] = {
        if (!canInline(morpheus, id)) throw new RefactorException("Can not be inlined.")

        val (fCalls, fDecls, fDefs, fCallsExpr) = getCallDeclDefCallExprs(id, morpheus)

        if (fDefs.isEmpty)
            Left("Inlining of external function definitions is not supported.")

        if (fCalls.exists(fCall => fDefs.exists(!isValidFDef(_, fCall._2, morpheus))))
            Left("Invalid selection")

        try {
            var tunitRefactored =
                fCalls.foldLeft(morpheus.getTranslationUnit)((curTunit, curFCall) =>
                    inlineFuncCallStmt(new Morpheus(curTunit, morpheus.getFM), curFCall._2, curFCall._1, fDefs))

            tunitRefactored =
                fCallsExpr.foldLeft(tunitRefactored)(
                    (curTunit, curFCall) => inlineFuncCallExpr(new Morpheus(curTunit, morpheus.getFM),
                        curFCall._2, curFCall._1, fDefs))

            // Remove old definition and declarations (note may cause linking error)
            if (!keepDeclaration) {
                tunitRefactored = fDefs.foldLeft(tunitRefactored)((curTunit, fDef) => remove(curTunit, fDef))
                tunitRefactored = fDecls.foldLeft(tunitRefactored)((curTunit, fDecl) => remove(curTunit, fDecl))
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
    def getCallDeclDefCallExprs(fCall: Id, morpheus: Morpheus): (List[(Id, Opt[Statement])], List[Opt[AST]],
        List[Opt[FunctionDef]], List[(Id, Opt[Expr])]) = {
        var fCallStmts = List[(Id, Opt[Statement])]()
        var fDecls = List[Opt[AST]]()
        var fDefs = List[Opt[FunctionDef]]()
        var fCallExprs = List[(Id, Opt[Expr])]()

        def checkExpr(expr: Option[Expr], id: Id) = expr.exists(filterAllASTElems[Id](_).exists(_.eq(id)))

        morpheus.getReferences(fCall).map(_.entry).foreach(id => {
            val parent = parentOpt(id, morpheus.getASTEnv)
            parent.entry match {
                case n: NestedFunctionDef =>
                    logger.info("Hit nested function def: " + n + " in " + id.getPositionFrom)
                    return (List(), List(), List(), List())
                case p: ParameterDeclaration =>
                    logger.info("Hit function pointer: " + p + " in " + id.getPositionFrom)
                    return (List(), List(), List(), List())
                case i: Initializer =>
                    logger.info("Hit function pointer initializer: " + i + " in " + id.getPositionFrom)
                    return (List(), List(), List(), List())
                // we have to take special care about control statements but not return statements
                case WhileStatement(expr, _) =>
                    if (checkExpr(Some(expr), id)) fCallExprs ::=(id, Opt(parent.feature, expr))
                    else fCallStmts ::=(id, parent.asInstanceOf[Opt[Statement]])
                case DoStatement(expr, _) =>
                    if (checkExpr(Some(expr), id)) fCallExprs ::=(id, Opt(parent.feature, expr))
                    else fCallStmts ::=(id, parent.asInstanceOf[Opt[Statement]])
                case f: ForStatement =>
                    if (checkExpr(f.expr1, id)) fCallExprs ::=(id, Opt(parent.feature, f.expr1.get))
                    else if (checkExpr(f.expr2, id)) fCallExprs ::=(id, Opt(parent.feature, f.expr2.get))
                    else if (checkExpr(f.expr3, id)) fCallExprs ::=(id, Opt(parent.feature, f.expr3.get))
                    else fCallStmts ::=(id, parent.asInstanceOf[Opt[Statement]])
                case SwitchStatement(expr, _) =>
                    if (checkExpr(Some(expr), id)) fCallExprs ::=(id, Opt(parent.feature, expr))
                    else fCallStmts ::=(id, parent.asInstanceOf[Opt[Statement]])
                case _: Statement => fCallStmts ::=(id, parent.asInstanceOf[Opt[Statement]])
                case _: FunctionDef => fDefs ::= parent.asInstanceOf[Opt[FunctionDef]]
                case InitDeclaratorI(_, _, None) => fDecls ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                case InitDeclaratorI(_, _, Some(_)) =>
                    parentAST(parentAST(id, morpheus.getASTEnv), morpheus.getASTEnv) match {
                        case a: ArrayAccess => throw new RefactorException("Hit invalid array access: " + a)
                        case _ => fCallStmts ::=(id, parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[DeclarationStatement]])
                    }
                case _: InitDeclaratorE => fDecls ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                case _: Expr =>
                    fCallExprs ::=(id, parent.asInstanceOf[Opt[Expr]])
                case _: NArySubExpr => fCallExprs ::=(id, parent.asInstanceOf[Opt[Expr]])
                case x =>
                    logger.error("Invalid function found! \n" + x + "\n" + id.getPositionFrom)
                    return (List(), List(), List(), List())
            }
        })

        // Sort fCall according their a appearence in the code to retrieve a clean picture for renamed variables and
        // to avoid complicate redeclaration checks for several functions calls in the same function
        (fCallStmts.sortWith((x, y) => comparePosition(x._1, y._1)), fDecls, fDefs, fCallExprs)
    }

    /*
     * We do not support the inline of function with variable amount of arguments like:
     * void foo(const char *bar, ...)
     */
    private def hasVArgsParameters(fDef: FunctionDef): Boolean =
        fDef.oldStyleParameters.exists(_.entry match {
            case v: VarArgs => true
            case _ => false
        })

    /*
     * Retrieves if a label such as case, goto, etc is right before the function call.
     * This check is required as iso-c99 does not allow for labels to appear immediately before a declaration.
     */
    private def hasLabelBeforeFCall(fCall: Id, morpheus: Morpheus): Boolean = {
        val parentOptStmt = parentOpt(fCall, morpheus.getASTEnv)
        pred(parentOptStmt, morpheus.getASTEnv).exists(prev => {
                prev.entry match {
                    case _: CaseStatement | _: GotoStatement | _: LabelStatement =>
                        prev.feature.and(parentOptStmt.feature).isSatisfiable(morpheus.getFM)
                        // only perform satisifiabilty check if precedessor is some kind of a label statement
                    case _ => false
                }
        })
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
       * int foo() { if (...) { return x+1;} else { return x+2; } // is compatible
       * int foo() { if (...) {return x;} ... return x; }         // is incompatible
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

        def getCCStatementForAstNode(i: AST): Option[CompoundStatement] = {
            findPriorASTElem[CompoundStatement](i, morpheus.getASTEnv) match {
                case None => None
                case Some(c) => Some(c)
            }
        }

        /**
         * Retrieves if the input statement is always part of the last statement of a compound statement.
         */
        def isPartOfLastStatementOfCompundStatement(fReturnStmt: CFGStmt, c: CompoundStatement): Boolean =
            isPartOf(fReturnStmt, c.innerStatements.last.entry) &&
                (getCCStatementForAstNode(parentAST(c, morpheus.getASTEnv)) match {
                    case Some(nc) => isPartOfLastStatementOfCompundStatement(fReturnStmt, nc)
                    case None => true
                })

        val fPreds = pred(fDef, morpheus.getASTEnv).filter(_.feature isSatisfiable morpheus.getFM)
        val fReturnStmts = fPreds.map(_.entry).filter { case _: ReturnStatement => true; case _ => false}


        fReturnStmts.exists(fReturnStmt => {
            val fStmt = getStatementForAstNode(fReturnStmt)
            val differentExitLocation = !fReturnStmts.diff(List(fReturnStmt)).forall(isPartOf(_, fStmt))

            val rPreds = pred(fReturnStmt, morpheus.getASTEnv).filter(_.feature isSatisfiable morpheus.getFM)

            val stmtAfterExit = rPreds.exists(rPred => {
                val rPCStmt = getCCStatementForAstNode(rPred.entry)
                rPCStmt match {
                    case None => false
                    case Some(c) =>
                        !isPartOfLastStatementOfCompundStatement(fReturnStmt, c)
                }

            })

            differentExitLocation || stmtAfterExit
        })
    }

    private def isRecursive(funcDef: FunctionDef): Boolean = {
        filterASTElems[PostfixExpr](funcDef).exists({
            case PostfixExpr(Id(name), FunctionCall(_)) => name == funcDef.getName
            case _ => false
        })
    }

    private def inlineFuncCallExpr(morpheus: Morpheus, fCall: Opt[Expr], fCallId: Id,
                                   fDefs: List[Opt[FunctionDef]]): TranslationUnit = {
        val compStmt = getCompStatement(fCall, morpheus.getASTEnv)

        val inlineCompStmtExprs = fDefs.flatMap(fDef =>
            Some(Opt(fDef.feature.and(fCall.feature), inlineFDefInExpr(compStmt, fDef, fCall, fCallId, morpheus))))

        val refactoredCompStmt =
            replaceExprWithCompStmExpr(compStmt, fCall.entry, mapCompStmtsToCompStmtExprs(inlineCompStmtExprs))

        replace(morpheus, compStmt, refactoredCompStmt)
    }

    // A compound statement expression only can hold one compound statement
    // to add several compound statements under different features in one expression we generate one big expression
    // and add the feature of the parent expression to its statements.
    private def mapCompStmtsToCompStmtExprs(compStmtExprs: List[Opt[CompoundStatementExpr]]): CompoundStatementExpr =
        CompoundStatementExpr(CompoundStatement(compStmtExprs.flatMap(compStmtExpr => {
            compStmtExpr.entry.compoundStatement.innerStatements.map(
                stmt => stmt.copy(feature = stmt.feature.and(compStmtExpr.feature)))
        })))

    private def inlineFuncCallStmt(morpheus: Morpheus, fCall: Opt[Statement], fCallId: Id,
                                   fDefs: List[Opt[FunctionDef]]): TranslationUnit = {

        def inlineFCallInIfStmt(fCallCompStmt: CompoundStatement): CompoundStatement = {
            val compundExprStmt = fDefs.map(fDef =>
                (inlineFDefInExprStmt(fCallCompStmt, morpheus, fCall, fCallId, fDef), fDef.feature.and(fCall.feature)))

            // Remove fCall and inline function definition
            replaceStmtWithStmtsInCompStmt(fCallCompStmt, fCall,
                compundExprStmt.map(inlinedFDef => Opt(inlinedFDef._2, inlinedFDef._1)))
        }

        def inlineFCallInCompStmt(fCallCompStmt: CompoundStatement): CompoundStatement = {
            val inlinedFCallCompStmt =
                fDefs.foldLeft(fCallCompStmt)((curStmt, fDef) => inlineFDefInCompStmt(curStmt, morpheus, fCall, fCallId, fDef))
            // Remove function call
            remove(inlinedFCallCompStmt, fCall)
        }

        val fCallCompStmt = getCompStatement(fCall, morpheus.getASTEnv)
        val inlinedFCallCompStmt = parentAST(fCall.entry, morpheus.getASTEnv) match {
            case _: CompoundStatement => inlineFCallInCompStmt(fCallCompStmt)
            case _: IfStatement | _: ElifStatement => inlineFCallInIfStmt(fCallCompStmt)
            case x => throw new RefactorException("No rule for inlining: " + x)
        }

        replace(morpheus, getCompStatement(fCall, morpheus.getASTEnv), inlinedFCallCompStmt)
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
                    case x => throw new RefactorException("No rule defined for:" + x)
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
                case x => throw new RefactorException("No assign rule for:" + x)
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
                case x => throw new RefactorException("Assign pattern not reached" + x)
            }
            // function to inline has been called as declaration, e.g int foo = functionToInline();
            case declStmt@DeclarationStatement(decl) =>
                returnStmts.foreach(statement => wStmt = initReturnStatement(decl, statement, declStmt, wStmt, fCall))
            // function to inline has been called like return functionToInline()
            case ReturnStatement(expr) =>
            // no initializing or special handling required;
            case x => throw new RefactorException("Pattern not reached " + x)
        }
        wStmt
    }

    private def inlineFDefInCompStmt(compStmt: CompoundStatement, morpheus: Morpheus, fCall: Opt[Statement],
                                     fCallId: Id, fDef: Opt[FunctionDef]): CompoundStatement = {
        var workingStatement = compStmt
        val idsToRename = getIdsToRename(fDef.entry, fCallId, workingStatement, morpheus)
        val (renamedIdsStmts, renamedIdsParams, idFeatureEnv) = renameShadowedIds(idsToRename, fDef, fCallId, fCall, morpheus)
        val initializer = getDeclarationsFromCallParameters(fCall, fCallId, renamedIdsParams, idFeatureEnv, morpheus)

        // apply feature environment
        val statements = mapFeaturesOnInlineStmts(renamedIdsStmts, fCall, morpheus)

        // find return statements
        val returnStmts = getReturnStmts(statements)

        // insert in tunit
        // Note: ISO-C99 does not allow a label to appear immediately before a declaration. For this case we add as
        // workaround a empty statement (";").
        if (hasLabelBeforeFCall(fCallId, morpheus)) workingStatement =
            insertListBefore(workingStatement, fCall, List(Opt(FeatureExprFactory.True, EmptyStatement())))
        workingStatement = insertListBefore(workingStatement, fCall, initializer)
        workingStatement = insertListBefore(workingStatement, fCall, statements)

        // insert return statements
        assignReturnValue(workingStatement, fCall, returnStmts, morpheus)
    }

    private def inlineFDefInExprStmt(compStmt: CompoundStatement, morpheus: Morpheus, fCall: Opt[AST], fCallId: Id,
                                     fDef: Opt[FunctionDef]): ExprStatement = {
        val compoundStmtExpr = inlineFDefInExpr(compStmt, fDef, fCall, fCallId, morpheus)

        fCall.entry match {
            case ExprStatement(PostfixExpr(_, FunctionCall(_))) => ExprStatement(compoundStmtExpr)
            case ExprStatement(AssignExpr(target, op, _)) => ExprStatement(AssignExpr(target, op, compoundStmtExpr))
            case x => throw new RefactorException("No rule defined for assigning for following return statement:" + x)
        }
    }


    private def inlineFDefInExpr(compStmt: CompoundStatement, fDef: Opt[FunctionDef],
                                 fCall: Opt[AST], fCallId: Id, morpheus: Morpheus): CompoundStatementExpr = {
        val idsToRename = getIdsToRename(fDef.entry, fCallId, compStmt, morpheus)

        val (renamedIdsStmts, renamedIdsParams, idFeatureEnv) = renameShadowedIds(idsToRename, fDef, fCallId, fCall, morpheus)
        val initializer = getDeclarationsFromCallParameters(fCall, fCallId, renamedIdsParams, idFeatureEnv, morpheus)
        val returnStmts = getReturnStmts(renamedIdsStmts)

        // replace (return expr; => expr;) or remove (return; => <removed>) return statements
        // the value of the last expression serves as the value of the entire construct.
        val stmtsToInline = returnStmts.foldLeft(renamedIdsStmts)((stmts, returnStmt) =>
            returnStmt.entry.expr match {
                case None => remove(stmts, returnStmt)
                case Some(_) => replace(stmts, returnStmt,
                    Opt(returnStmt.feature, ExprStatement(returnStmt.entry.expr.get)))
            })
        CompoundStatementExpr(
            CompoundStatement(initializer ::: mapFeaturesOnInlineStmts(stmtsToInline, fCall, morpheus)))
    }

    /**
     * Determines if the identifier id occurs in different scopes.
     * As renaming of globally scoped variables causes to alter the behaviour, we do not allow inlining of functions
     * containing identifiers of variables which are not in the same scope under each condition.
     *
     * Example for incompatible variable (variable i) scoping:
     *
     * int i;
     *
     * int foo() {
     * #ifdef A
     * int i;
     * #endif
     * i = 5;
     * }
     */
    private def hasIncompatibleVariableScoping(id: Id, compStmt: CompoundStatement, morpheus: Morpheus): Boolean = {
        val env = morpheus.getEnv(compStmt.innerStatements.last.entry)
        val condScope = env.varEnv.lookupScope(id.name)

        condScope match {
            case One(scope) => false
            case _ => ConditionalLib.leaves(condScope).distinct.tail.nonEmpty
        }
    }

    private def isValidFDef(fDef: Opt[FunctionDef], fCall: Opt[_], morpheus: Morpheus): Boolean = {
        // If a function call's feature does not imply a function definition's feature,
        // then there is no need to inline this definition at this call.

        if (!fCall.feature.implies(fDef.feature).isSatisfiable(morpheus.getFM))
            return false

        if (isRecursive(fDef.entry))
            throw new RefactorException("Can not inline - method is recursive.")
        if (hasIncompatibleCFG(fDef.entry, morpheus))
            throw new RefactorException("Can not inline - method has bad return statements")
        if (hasVArgsParameters(fDef.entry))
            throw new RefactorException("Can not inline - method has variable arguments")

        true
    }

    private def getIdsToRename(fDEf: FunctionDef, fCallId: Id, compStmt: CompoundStatement, morpheus: Morpheus) = {
        val idsToInline = filterAllASTElems[Id](fDEf)
        idsToInline.filter(isDeclaredInFunctionCallScope(_, fCallId, compStmt, morpheus))
    }

    private def isFDefOrFDecl(id: Id, morpheus: Morpheus): Boolean = {
        try {
            if (morpheus.getScopeEnv(id).varEnv.lookupType(id.name).forall(_.isFunction))
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
            val specifier = findPriorASTElem[StructOrUnionSpecifier](ref, morpheus.getASTEnv) match {
                case Some(s@StructOrUnionSpecifier(_, Some(id), _, _, _)) =>
                    id.eq(ref) && {
                        findPriorASTElem[ParameterDeclaration](s, morpheus.getASTEnv) match {
                            case Some(_) => true
                            case _ => false
                        }
                    }
                case _ => false
            }

            val decl = findPriorASTElem[StructDeclarator](ref, morpheus.getASTEnv) match {
                case Some(_) => true
                case _ => false
            }

            specifier || decl
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
    private def isDeclaredInFunctionCallScope(inlineId: Id, fCallId: Id, callCompStmt: CompoundStatement, morpheus: Morpheus): Boolean = {
        val nameWithFeature = Opt(morpheus.getASTEnv.featureExpr(inlineId), inlineId.name)

        // id is part of a jump statement? if so, check if the same jump location already exists
        if (findPriorASTElem[Statement](inlineId, morpheus.getASTEnv) match {
            case Some(LabelStatement(_,_)) | Some(GotoStatement(_)) =>
                nameIsPartOfJump(inlineId.name, callCompStmt, morpheus)
            case _ => false
        })
            return true


        // lookup if name is visible in current scope
        if (!isVisibleNameInFunctionScope(nameWithFeature, fCallId, callCompStmt, morpheus))
            return false

        // check if any reference of the inline id links into the calling scope - if so no renaming is required
        val idReferences = morpheus.getReferences(inlineId)
        val idsInCompStmt = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        filterAllASTElems[Id](callCompStmt) foreach idsInCompStmt.add

        if (idReferences.exists(idsInCompStmt.contains))
            return false

        // in both places the has the same global visibility -> no need to rename
        val fDefCompStmt = getCompStatement(parentOpt(inlineId, morpheus.getASTEnv).asInstanceOf[Opt[AST]], morpheus.getASTEnv)
        if ((fDefCompStmt != null) && isVisibleGlobalNameInFunctionScope(nameWithFeature, fCallId, fDefCompStmt, morpheus))
            return false

        // id is function -> no rename
        if (isFDefOrFDecl(inlineId, morpheus))
            return false

        // id is part of struct -> no rename
        if (isPartOfStruct(idReferences, morpheus))
            return false

        // id is typedef -> no rename
        if (isTypeDef(idReferences, morpheus))
            return false

        true
    }

    private def nameIsPartOfJump(jump: String, compStmt: CompoundStatement, morpheus: Morpheus): Boolean =
        filterAllASTElems[Id](compStmt, morpheus.getASTEnv).par.exists(cId => {
            findPriorASTElem[Statement](cId, morpheus.getASTEnv) match {
                case Some(LabelStatement(Id(name), _)) => name == jump
                case Some(GotoStatement(target)) => filterAllASTElems[Id](target).exists(_.name == jump)
                case _ => false
            }
        })

    /**
     * Checks if a symbol is visible at the place of the inlining function scope.
     */
    private def isVisibleGlobalNameInFunctionScope(name: Opt[String], fCallId: Id, callCompStmt: CompoundStatement, morpheus: Morpheus) =
        isPartOfScope(name, fCallId, callCompStmt, morpheus, 0)

    /**
     * Checks if a symbol is visible at the place of the inlining function scope.
     */
    private def isVisibleNameInFunctionScope(symbol: Opt[String], fCallId: Id, callCompStmt: CompoundStatement, morpheus: Morpheus) =
        !isPartOfScope(symbol, fCallId, callCompStmt, morpheus, -1, true)

    /**
     * Checks if a symbol is part of a specific scope.
     */
    private def isPartOfScope(symbol: Opt[String], fCallId: Id, compStmt: CompoundStatement, morpheus: Morpheus,
                              scope: Int, forall: Boolean = false): Boolean = {
        val csEnv = morpheus.getEnv(compStmt.innerStatements.last.entry)
        val csNameScope = csEnv.varEnv.lookupScope(symbol.entry)

        val callEnv = morpheus.getEnv(fCallId)
        val callNameScope = callEnv.varEnv.lookupScope(symbol.entry)

        if (forall)
            ConditionalLib.items(csNameScope).forall(cond =>
                (cond._2 == scope) && symbol.feature.and(cond._1).isSatisfiable(morpheus.getFM)) &&
                ConditionalLib.items(callNameScope).forall(cond =>
                    (cond._2 == scope) && symbol.feature.and(cond._1).isSatisfiable(morpheus.getFM))
        else
            ConditionalLib.items(csNameScope).exists(cond =>
                (cond._2 == scope) && symbol.feature.and(cond._1).isSatisfiable(morpheus.getFM)) ||
                ConditionalLib.items(callNameScope).exists(cond =>
                    (cond._2 == scope) && symbol.feature.and(cond._1).isSatisfiable(morpheus.getFM))
    }

    /**
     * Converts the parameters of a function call into declarations for the inlined function.
     * e.g:
     *
     * void foo(int a, int b) {
     * a + b;
     * }
     *
     * void bar() {
     * int i = 5;
     * int j = 6;
     *
     * foo(i, j);
     *
     * }
     *
     * would generate the following declarations for the parameters a and b of foo:
     *
     * int a = i;
     * int b = j;
     *
     */
    private def getDeclarationsFromCallParameters(fCallStmt: Opt[AST], fCallId: Id, parameters: List[Opt[DeclaratorExtension]],
                                                  idFeatureEnv: java.util.IdentityHashMap[Id, FeatureExpr],
                                                  morpheus: Morpheus): List[Opt[DeclarationStatement]] =
        findPriorASTElem[PostfixExpr](fCallId, morpheus.getASTEnv) match {
            case Some(fCall) =>
                fCall.s match {
                    case f: FunctionCall =>
                        parameters.flatMap(generateDeclarationsFromCallParamExpr(_, f.params.exprs, idFeatureEnv, morpheus))
                    case _ =>
                        throw new RefactorException("Invalid function call has been selected: " + fCallStmt + "\n" + fCallId)
                }
            case None =>
                throw new RefactorException("Invalid function call has been selected: " + fCallStmt + "\n" + fCallId)
        }

    /*
     * Helping function to map the function call parameter to the corresponding inline function parameter
     * and generate its declaration.
     */
    private def generateDeclarationsFromCallParamExpr(parameters: Opt[DeclaratorExtension], callExprs: List[Opt[Expr]],
                                                      idFeatureEnv: java.util.IdentityHashMap[Id, FeatureExpr],
                                                      morpheus: Morpheus): List[Opt[DeclarationStatement]] = {
        var callParams = callExprs
        var declStmts: List[Opt[DeclarationStatement]] = List()
        parameters.entry match {
            case DeclParameterDeclList(paramDecls) =>
                declStmts = paramDecls.flatMap(convertParameterToDeclaration)
            case missed =>
                throw new RefactorException("No rule defined for converting parameter to initializer:" + missed)
        }

        def convertParameterToDeclaration(paramDecl: Opt[ParameterDeclaration]): List[Opt[DeclarationStatement]] = {
            if (callParams.isEmpty)
                if (paramDecl.entry.specifiers.exists({ case x: Opt[VoidSpecifier] => true})) return List()
                else throw new RefactorException("Failed to correctly map call parameters with function parameters.")

            val currentCallParam = callParams.head
            val paramDeclFeature =
                paramDecl.entry match {
                    case p: ParameterDeclarationD =>
                        // For renamed parameters we are unable to determine its feature expression by using the astEnv,
                        // therefore when the renaming takes place we generated a new renamed id to feature map and fallback
                        // to this map in case a renamed parameter occurs.
                        if (idFeatureEnv.containsKey(p.decl.getId)) idFeatureEnv.get(p.decl.getId)
                        else morpheus.getASTEnv.featureExpr(paramDecl)
                    case unknown =>
                        throw new RefactorException("Unknown mapping for parameter: " + unknown)
                }

            val currentCallParamFeature = morpheus.getASTEnv.featureExpr(currentCallParam.entry)
            val declFeature = currentCallParamFeature.and(paramDeclFeature)

            if (!declFeature.isSatisfiable(morpheus.getFM)) List()
            else {
                // call parameter and function parameter have matched - remove call parameter from working list
                callParams = callParams.tail
                paramDecl.entry match {
                    case p: ParameterDeclarationD =>
                        val specifier = p.specifiers.map(spec => spec.copy(feature = declFeature.and(spec.feature)))

                        // Remove decl array access as arrays are not assignable.
                        // To preserve to code correctness we add an additional pointer.
                        // Invalid:
                        //  char foo[50] = "Hello, world!";
                        //  char bar[50] = foo;
                        // Valid:
                        //  char foo[50] = "Hello, world!";
                        //  char *bar = foo;
                        val (declExt, declPointers) = p.decl.extensions.foldLeft(
                            (List[Opt[DeclaratorExtension]](), p.decl.pointers))((result, extension) => {
                            extension.entry match {
                                case d: DeclArrayAccess =>
                                    (result._1, result._2 :+ Opt(extension.feature, Pointer(List())))
                                case _ => (result._1 :+ extension, result._2)
                            }
                        })

                        val declStmt = List(Opt(declFeature,
                            DeclarationStatement(
                                Declaration(specifier, List(Opt(declFeature,
                                    InitDeclaratorI(AtomicNamedDeclarator(declPointers, p.decl.getId, declExt), List(),
                                        Some(Initializer(None, currentCallParam.entry)))))))))

                        val nextCallParamIsAlternative : Boolean =
                            if (callParams.nonEmpty) {
                                val nextCallParam = callParams.head
                                val nextCallParamFeature = morpheus.getASTEnv.featureExpr(nextCallParam.entry)

                                val nextDeclFeature = nextCallParamFeature.and(paramDeclFeature)

                                if (nextDeclFeature.isSatisfiable(morpheus.getFM))
                                    !nextCallParamFeature.and(currentCallParamFeature).isSatisfiable(morpheus.getFM)
                                else false
                            }
                            else false

                        if (nextCallParamIsAlternative) declStmt ::: convertParameterToDeclaration(paramDecl)
                        else declStmt
                    case missed =>
                        throw new RefactorException("No rule defined for initializing parameter:" + missed)
                }
            }
        }

        declStmts
    }

    private def renameShadowedIds(idsToRename: List[Id], fDef: Opt[FunctionDef], fCallId: Id, fCall: Opt[AST],
                                  morpheus: Morpheus):
    (List[Opt[Statement]], List[Opt[DeclaratorExtension]], java.util.IdentityHashMap[Id, FeatureExpr]) = {

        def generateValidNewName(id: Id, stmt: Opt[AST], morpheus: Morpheus, appendix: Int = 1): String = {
            val newName = id.name + "_" + appendix
            if (!isValidInModule(newName, stmt.entry, morpheus)
                || isVisibleNameInFunctionScope(
                Opt(stmt.feature, newName), fCallId, getCompStatement(fCall, morpheus.getASTEnv), morpheus)
                || isVisibleGlobalNameInFunctionScope(
                Opt(stmt.feature, newName), fCallId, getCompStatement(fCall, morpheus.getASTEnv), morpheus)
                || isOccupiedByIdentifier(newName, getCompStatement(fCall, morpheus.getASTEnv))
                || nameIsPartOfJump(newName, fDef.entry.stmt, morpheus))
                generateValidNewName(id, stmt, morpheus, appendix + 1)

            else newName
        }

        // We need to know the feature condition of each id used as parameter in the function we want to inline.
        // In case we rename such an parameter, astEnv.featureExpr would fail to determine the feature of the parameter id.
        // Therefor we store the feature expression of the original parameter for the renamed one in a id -> feature map.
        val paramFeatureEnv = new java.util.IdentityHashMap[Id, FeatureExpr]()

        def rename[T <: AST](o: List[Opt[T]], id: Id) = {
            val renamedId = id.copy(name = generateValidNewName(id, fCall, morpheus))
            paramFeatureEnv.put(renamedId, morpheus.getASTEnv.featureExpr(parentAST(id, morpheus.getASTEnv)))
            replace(o, id, renamedId)
        }

        val statements = idsToRename.foldLeft(fDef.entry.stmt.innerStatements)((statement, id) => rename(statement, id))
        val parameters = idsToRename.foldLeft(fDef.entry.declarator.extensions)((extension, id) => rename(extension, id))

        (statements, parameters, paramFeatureEnv)
    }

    private def mapFeaturesOnInlineStmts(statements: List[Opt[Statement]], call: Opt[AST],
                                         morpheus: Morpheus): List[Opt[Statement]] =
        statements.flatMap(statement => {
            val feature = statement.feature.and(call.feature)
            feature.isSatisfiable(morpheus.getFM) match {
                case true => Some(statement.copy(feature = feature))
                case _ => None
            }
        })

    private def getReturnStmts(statements: List[Opt[Statement]]): List[Opt[ReturnStatement]] =
        filterAllOptElems(statements).filter {
            case Opt(_, _: ReturnStatement) => true
            case _ => false
        }.asInstanceOf[List[Opt[ReturnStatement]]]

    private def isFunctionCall(morpheus: Morpheus, id: Id): Boolean =
        parentAST(id, morpheus.getASTEnv) match {
            case PostfixExpr(`id`, FunctionCall(_)) => true
            case _ => false
        }

    private def isOccupiedByIdentifier(name: String, compStmt: CompoundStatement) =
        filterAllASTElems[Id](compStmt).exists(_.name == name)
}