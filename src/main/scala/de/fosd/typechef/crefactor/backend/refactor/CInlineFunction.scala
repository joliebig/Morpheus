package de.fosd.typechef.crefactor.backend.refactor

import de.fosd.typechef.crefactor._
import backend.ASTSelection
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.typesystem._
import de.fosd.typechef.conditional._
import scala._
import scala.Some
import de.fosd.typechef.crefactor.frontend.util.Selection

/**
 * Implements the technique of inlining a function
 */
object CInlineFunction extends ASTSelection with CRefactor {

    def getSelectedElements(morpheus: Morpheus, selection: Selection): List[AST] = {
        val functions = (filterASTElems[FunctionDef](morpheus.getTranslationUnit) ::: filterASTElems[FunctionCall](morpheus.getTranslationUnit)
            ::: filterAllASTElems[NestedFunctionDef](morpheus.getTranslationUnit)).filter(x => isSelected(x, morpheus.getASTEnv, selection))
        filterASTElementsForFile(functions, selection.getFilePath).sortWith(comparePosition)
    }

    def getAvailableIdentifiers(morpheus: Morpheus, selection: Selection): List[Id] = {
        val ids = getSelectedElements(morpheus, selection).map(x => getFunctionIdentifier(x, morpheus.getASTEnv))
        ids.sortWith(comparePosition)
    }

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean =
        !getAvailableIdentifiers(morpheus, selection).isEmpty

    def isAvailable(morpheus: Morpheus, call: Id): Boolean = {
        if (!isFunctionCall(morpheus, call)) return false

        val defs = divideCallDeclDef(call, morpheus)._3
        if (defs.isEmpty)
            return false

        // compatible control flow or recursive
        if (defs.exists(func => badReturnStatement(func.entry, morpheus)
            || isRecursive(func.entry)))
            return false

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
     * @param rename indicates if variables should be renamed
     * @return the refactored tunit
     */
    def inline(morpheus: Morpheus, id: Id, rename: Boolean, evalMode: Boolean, once: Boolean = false):
    TranslationUnit = {
        val (calls, decl, fDefs, callExpr) = divideCallDeclDef(id, morpheus)
        
        // TODO rewrite to Either[String, TranslationUnit]; similar to the other refactorings!
        if (fDefs.isEmpty)
            assert(false, "Inlining of external function definitions is not supported.")

        var tunitRefactored = calls.foldLeft(morpheus.getTranslationUnit)((curTunit, call) => 
            inlineFuncCall(curTunit, new Morpheus(curTunit), call, fDefs, rename))
        tunitRefactored = 
            callExpr.foldLeft(tunitRefactored)(
                (workingAST, expr) => inlineFuncCallExpr(workingAST, new Morpheus(workingAST),
                    expr, fDefs, rename))

        // Remove inlined function stmt's declaration and definitions
        if (!evalMode)
            tunitRefactored = removeFuncDeclDefsFromAST(tunitRefactored, decl, fDefs)
        tunitRefactored
    }


    private def removeFuncDeclDefsFromAST(tunit: TranslationUnit, decl: List[Opt[AST]],
                                          defs: List[Opt[FunctionDef]]): TranslationUnit = {
        val astWoDecl = decl.foldLeft(tunit)((curTunit, x) => removeFromAST(curTunit, x))
        val astWoDeclDef = defs.foldLeft(astWoDecl)((curTunit, x) => removeFromAST(curTunit, x))
        astWoDeclDef
    }

    def divideCallDeclDef(callId: Id, morpheus: Morpheus): (List[Opt[Statement]], List[Opt[AST]],
        List[Opt[FunctionDef]], List[Opt[AST]]) = {
        var callStmt = List[Opt[Statement]]()
        var decl = List[Opt[AST]]()
        var defs = List[Opt[FunctionDef]]()
        var callExpr = List[Opt[AST]]()

        morpheus.linkage(callId).map(_.entry).foreach(id => {
            val parent = parentOpt(id, morpheus.getASTEnv)
            parent.entry match {
                case w: WhileStatement => callExpr ::= parent.asInstanceOf[Opt[AST]]
                case p: Statement => callStmt ::= parent.asInstanceOf[Opt[Statement]]
                case f: FunctionDef => defs ::= parent.asInstanceOf[Opt[FunctionDef]]
                // case n: NestedFunctionDef => defs = n :: defs // TODO Nested FunctionDefs => Can this happen?
                case iI: InitDeclaratorI =>
                    iI.i match {
                        case None => decl ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                        case _ => parentAST(parentAST(id, morpheus.getASTEnv), morpheus.getASTEnv) match {
                            case a: ArrayAccess => println("array " + a + " " + parent) // TODO ArrayAccess
                            case _ => callStmt ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[DeclarationStatement]]
                        }
                    }
                case iE: InitDeclaratorE => decl ::= parentOpt(parent, morpheus.getASTEnv).asInstanceOf[Opt[AST]]
                case e: Expr => callExpr ::= parent.asInstanceOf[Opt[AST]]
                case n: NArySubExpr => callExpr ::= parent.asInstanceOf[Opt[AST]]
                case _ => assert(false, "Invalid function found!")
            }
        })

        (callStmt, decl, defs, callExpr)
    }

    private def getFunctionIdentifier(function: AST, astEnv: ASTEnv): Id = {
        function match {
            case f: FunctionDef => f.declarator.getId
            case n: NestedFunctionDef => n.declarator.getId
            case c: FunctionCall => getFunctionIdentifier(astEnv.parent(c).asInstanceOf[AST], astEnv)
            case PostfixExpr(i@Id(_), _) => i
            case _ =>
                assert(false, function)
                null
        }
    }

    private def isSelected(element: AST, astEnv: ASTEnv, selection: Selection): Boolean = {
        element match {
            case f: FunctionDef => isPartOfSelection(f.declarator.getId, selection)
            case n: NestedFunctionDef => isPartOfSelection(n.declarator.getId, selection)
            case c: FunctionCall => isSelected(astEnv.parent(c).asInstanceOf[AST], astEnv, selection)
            case p: PostfixExpr => isPartOfSelection(p.p, selection)
            case _ =>
                assert(false, element)
                false
        }
    }

    /*
     * Retrieves if there are bad conditional return during control flow.
     */
    private def badReturnStatement(func: FunctionDef, morpheus: Morpheus): Boolean = {

        // TODO Optimize Runtime
        def codeAfterStatement(feature: FeatureExpr, opt: Opt[_]): Boolean = {
            val next = nextOpt(opt, morpheus.getASTEnv)
            next match {
                case null => false
                case _ =>
                    if ((feature.equivalentTo(FeatureExprFactory.True)
                        || feature.implies(next.feature).isTautology(morpheus.getFM))) true
                    else codeAfterStatement(feature, next)
            }
        }

        def getStatementOpt(opt: Opt[_], statements: List[Opt[Statement]]): Opt[_] = {
            statements.foreach(statement => if (filterAllOptElems(statement).exists(x => opt.eq(x))) return statement)
            null
        }

        def getStatementOpts(opt: Opt[_], statement: Product, statements: List[Opt[_]] = List[Opt[_]]()): List[Opt[_]] = {
            var result = statements
            val filtered = filterAllOptElems(statement)
            if (filtered.length > 1) filtered.foreach(stmt =>
                if (filterAllOptElems(stmt).exists(x =>
                    opt.eq(x))) result :::= stmt :: getStatementOpts(opt, stmt.entry.asInstanceOf[Product], result))
            result
        }

        filterASTElems[ReturnStatement](func).exists(statement =>
            statement match {
                case r: ReturnStatement =>
                    val parent = parentOpt(statement, morpheus.getASTEnv)
                    val outerStatement = getStatementOpt(parent, func.stmt.innerStatements)
                    getStatementOpts(parent, outerStatement, List(outerStatement)).exists(statement => codeAfterStatement(parent.feature, statement))
                case _ => false
            })
    }

    private def isRecursive(funcDef: FunctionDef): Boolean = {
        filterASTElems[PostfixExpr](funcDef).exists(expr => expr match {
            case PostfixExpr(Id(name), FunctionCall(_)) => name.equals(funcDef.getName)
            case _ => false
        })
    }

    private def isRecursive(funcDef: NestedFunctionDef): Boolean = {
        filterASTElems[PostfixExpr](funcDef).exists(expr => expr match {
            case PostfixExpr(Id(name), FunctionCall(_)) => name.equals(funcDef.getName)
            case _ => false
        })
    }

    private def inlineFuncCallExpr(ast: AST, morpheus: Morpheus, call: Opt[AST],
                                   funcDefs: List[Opt[_]], rename: Boolean): TranslationUnit = {
        val workingCallCompStmt = getCallCompStatement(call, morpheus.getASTEnv)

        def generateInlineExprStmts: List[(CompoundStatementExpr, FeatureExpr)] = {
            funcDefs.flatMap(funcDef => funcDef match {
                case f: Opt[FunctionDef] =>
                    val inlineExpr = inlineFuncDefInExpr(workingCallCompStmt, f, call, morpheus, rename)
                    inlineExpr match {
                        case null => None
                        case _ => Some(CompoundStatementExpr(inlineExpr), funcDef.feature.and(call.feature))
                    }
            })
        }

        def inlineInCorrectConditionalStmt(current: Conditional[Expr], call: AST, expr: AST, inlineChoice: CompoundStatementExpr): Conditional[Expr] = {

            def inlineInOne(entry: Expr, expr: AST, o: One[Expr], call: AST, inlineChoice: CompoundStatementExpr): Conditional[Expr] = {
                if (!entry.eq(expr)) return o
                expr match {
                    case NAryExpr(e, others) =>
                        call match {
                            case n@NArySubExpr(op, _) =>
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

            current match {
                case o@One(entry) =>
                    inlineInOne(entry, expr, o, call, inlineChoice)
                case c@Choice(_, c1@Choice(_, _, _), c2@Choice(_, _, _)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(c1, call, expr, inlineChoice), elseBranch = inlineInCorrectConditionalStmt(c2, call, expr, inlineChoice))
                case c@Choice(_, o1@One(_), c2@Choice(_, _, _)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(o1, call, expr, inlineChoice), elseBranch = inlineInCorrectConditionalStmt(c2, call, expr, inlineChoice))
                case c@Choice(_, c1@Choice(_, _, _), o2@One(_)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(c1, call, expr, inlineChoice), elseBranch = inlineInCorrectConditionalStmt(o2, call, expr, inlineChoice))
                case c@Choice(_, o1@One(_), o2@One(_)) =>
                    c.copy(thenBranch = inlineInCorrectConditionalStmt(o1, call, expr, inlineChoice), elseBranch = inlineInCorrectConditionalStmt(o2, call, expr, inlineChoice))
                case _ => current
            }
        }

        def inlineInExpr(expr: Expr, inlineExprStatements: List[(CompoundStatementExpr, FeatureExpr)]) = replaceInAST(expr, call.entry, buildVariableCompoundStatement(inlineExprStatements)).asInstanceOf[Expr]

        findPriorASTElem[Statement](call.entry, morpheus.getASTEnv) match {
            case None =>
                assert(false, "This should not have happend!")
                null
            case entry =>
                val inlineExprStatements = generateInlineExprStmts
                val parent = parentOpt(entry.get, morpheus.getASTEnv)
                var replaceStmt: CompoundStatement = null
                var callParent = parentAST(call.entry, morpheus.getASTEnv)
                entry.get match {
                    case i@IfStatement(c@condition, _, e@elifs, _) =>
                        // TODO Refactor for performance
                        if (callParent.eq(i)) callParent = call.entry
                        val cond = inlineInCorrectConditionalStmt(condition, call.entry, callParent, buildVariableCompoundStatement(inlineExprStatements))
                        if (!cond.eq(i.condition)) replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = i.copy(condition = cond)))
                        else {
                            val refactoredElifs = elifs.map(elif => {
                                if (parentAST(call.entry, morpheus.getASTEnv).eq(elif.entry)) callParent = call.entry
                                val condition = elif.entry.condition
                                val cond = inlineInCorrectConditionalStmt(condition, call.entry, callParent, buildVariableCompoundStatement(inlineExprStatements))
                                if (!cond.eq(condition))
                                    elif.copy(entry = elif.entry.copy(condition = cond))
                                else elif
                            })
                            replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = i.copy(elifs = refactoredElifs)))
                        }
                    case w: WhileStatement =>
                        replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = w.copy(expr = inlineInExpr(w.expr, inlineExprStatements))))
                    case s: SwitchStatement =>
                        replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = s.copy(expr = inlineInExpr(s.expr, inlineExprStatements))))
                    case d: DoStatement =>
                        replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = d.copy(expr = inlineInExpr(d.expr, inlineExprStatements))))
                    case e: ExprStatement =>
                        replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = e.copy(expr = inlineInExpr(e.expr, inlineExprStatements))))
                    case c: CaseStatement =>
                        replaceStmt = replaceInASTOnceTD(workingCallCompStmt, parent, parent.copy(entry = c.copy(c = inlineInExpr(c.c, inlineExprStatements))))
                    case x =>
                        println("Missed InlineStatementExpr" + x)
                        assert(false, "Refactoring failed - missed InlineStatementExpr")
                }
                insertRefactoredAST(morpheus, getCallCompStatement(call, morpheus.getASTEnv),
                    replaceStmt)
        }
    }

    private def inlineFuncCall(tunit: TranslationUnit, morpheus: Morpheus, call: Opt[Statement],
                               funcDefs: List[Opt[_]], rename: Boolean): TranslationUnit = {
        var workingCallCompStmt = getCallCompStatement(call, morpheus.getASTEnv)

        def inlineIfStmt(funcDefs: List[Opt[_]], callCompStmt: CompoundStatement): CompoundStatement = {
            val inlineExprStatements = funcDefs.flatMap(funcDef => funcDef match {
                case f: Opt[FunctionDef] =>
                    val inlineStmt = inlineFuncDefInExprStmt(callCompStmt, morpheus, call, f, rename)
                    inlineStmt match {
                        case null => None
                        case _ => Some(inlineStmt, funcDef.feature.and(call.feature))
                    }
                case _ =>
                    println("Forgotten definition")
                    None
            })
            // Remove fCall and inline function
            replaceInAST(callCompStmt, morpheus.getASTEnv.parent(call.entry),
                buildChoice(inlineExprStatements)).asInstanceOf[CompoundStatement]
        }

        def inlineCompStmt(fDefs: List[Opt[_]], callCompStmt: CompoundStatement):
        CompoundStatement = {
            val workingCallCompStmt = 
                fDefs.foldLeft(callCompStmt)(
                    (curStmt, fDef) => fDef match {
                        case f: Opt[FunctionDef] =>
                            inlineFuncDefInCompStmt(curStmt, morpheus, call, f, rename)
                        case _ =>
                            println("Forgotten definition")
                            curStmt
            })
            // Remove stmt
            removeFromAST(workingCallCompStmt, call)
        }

        parentAST(call.entry, morpheus.getASTEnv) match {
            case c: CompoundStatement =>
                workingCallCompStmt = inlineCompStmt(funcDefs, workingCallCompStmt)
            case i: IfStatement => workingCallCompStmt = inlineIfStmt(funcDefs, workingCallCompStmt)
            case e: ElifStatement => workingCallCompStmt = inlineIfStmt(funcDefs, workingCallCompStmt)
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
        var workingStatement = statement

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
                    spec.copy(feature = feature)}
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
                    // TODO Ask Jörg for better solution -> manybu/oncetd <- wtf!?!!?!
                        if (wStatement.innerStatements.exists(entry => entry.eq(statement)))
                            wStatement = replaceInASTOnceTD(wStatement, stmt,
                                Opt(feature, ExprStatement(AssignExpr(t, o, entry))))
                        else wStatement = replaceInAST(wStatement, stmt,
                            Opt(feature, ExprStatement(AssignExpr(t, o, entry))))
                    else wStatement = removeFromAST(wStatement, stmt)
                case _ => assert(false, "Missed Pattern!")
            }
            wStatement
        }

        def includeReturnStatement(returnStmts: List[Opt[Statement]], cStatement: CompoundStatement, call: Opt[Statement], assign: Boolean): CompoundStatement = {
            var wStatement = cStatement
            returnStmts.foreach(statement => statement.entry match {
                case ReturnStatement(Some(entry)) =>
                    val feature = statement.feature.and(call.feature)
                    if (assign) wStatement = assignStatement(statement, wStatement, call, entry)
                    else if (feature.isSatisfiable(morpheus.getFM))
                        wStatement = replaceInASTOnceTD(wStatement, statement,
                            Opt(feature, ExprStatement(entry)))
                case _ => assert(false, "Missed Pattern!")
            })
            wStatement
        }

        call.entry match {
            case expr@ExprStatement(e) => e match {
                case p: PostfixExpr => workingStatement = includeReturnStatement(returnStmts, workingStatement, call, false)
                case a: AssignExpr => workingStatement = includeReturnStatement(returnStmts, workingStatement, call, true)
                case x => println("missed" + x)
            }
            case declStmt@DeclarationStatement(decl) => returnStmts.foreach(statement => workingStatement = initReturnStatement(decl, statement, declStmt, workingStatement, call))
            case x =>
                println("missed " + x)
                assert(false, "Pattern matching not exhaustive")
        }
        workingStatement
    }

    private def inlineFuncDefInCompStmt(compoundStmt: CompoundStatement,
                                        morpheus: Morpheus, fCall: Opt[Statement],
                                        fDef: Opt[FunctionDef], rename: Boolean):
    CompoundStatement = {
        if (!isValidFDef(fDef, fCall, compoundStmt, morpheus))
            return compoundStmt
        var workingStatement = compoundStmt

        val idsToRename = getIdsToRename(fDef.entry, fCall.entry, workingStatement, morpheus)
        if (!rename && !idsToRename.isEmpty)
            assert(false, "Can not inline - variables need to be renamed")

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

    private def inlineFuncDefInExprStmt(compoundStmt: CompoundStatement, morpheus: Morpheus,
                                        fCall: Opt[AST], fDef: Opt[FunctionDef], rename: Boolean):
    ExprStatement = {
        if (!isValidFDef(fDef, fCall, compoundStmt, morpheus))
            return null

        val compoundStmtExpr: CompoundStatementExpr =
            CompoundStatementExpr(inlineFuncDefInExpr(compoundStmt, fDef, fCall, morpheus, rename))

        fCall.entry match {
            case ExprStatement(PostfixExpr(pe, FunctionCall(_))) =>
                ExprStatement(compoundStmtExpr)
            case ExprStatement(AssignExpr(target, op, _)) =>
                ExprStatement(AssignExpr(target, op, compoundStmtExpr))
            case _ =>
                assert(false, "An error occurred. Unable to assign return statements")
                null
        }
    }


    private def inlineFuncDefInExpr(compoundStmt: CompoundStatement, fDef: Opt[FunctionDef],
                                    fCall: Opt[AST], morpheus: Morpheus, rename: Boolean):
    CompoundStatement = {
        var workingStatement = compoundStmt

        val idsToRename = getIdsToRename(fDef.entry, fCall.entry, workingStatement, morpheus)
        if (!rename && !idsToRename.isEmpty)
            assert(false, "Inline function not possible - some variables need to be renamed")

        val renamed = renameShadowedIds(idsToRename, fDef, fCall, morpheus)
        val initializer = getInitializers(fCall, renamed._2, morpheus)
        var statements = applyFeaturesOnInlineStmts(renamed._1, fCall, morpheus)
        val returnStmts = getReturnStmts(statements)

        // remove return statements
        statements = returnStmts.foldLeft(statements)((stmts, returnStmt) =>
            returnStmt.entry.expr match {
                case None => removeFromAST(stmts, returnStmt)
                case Some(_) => replaceInAST(stmts, returnStmt, Opt(returnStmt.feature, ExprStatement(returnStmt.entry.expr.get)))
            })
        CompoundStatement(initializer ::: statements)
    }

    private def isDeclared(id: Id, env: Env, statement: CompoundStatement, morpheus: Morpheus):
    Boolean = {

        def checkOne(one: Conditional[(CType, DeclarationKind, Int, Linkage)],
                     recursive: Boolean = false): Boolean = {
            one match {
                case One(x) =>
                    if (x._1.isUnknown) {
                        if (!recursive)
                            checkConditional(morpheus.getEnv(statement.innerStatements.last.entry).varEnv.lookup(id.name), true)
                        else false
                    } else if (x._1.isFunction) {
                        parentAST(id, morpheus.getASTEnv) match {
                            case PostfixExpr(_, FunctionCall(_)) => false
                            case _ => parentOpt(id, morpheus.getASTEnv).entry match {
                                case f: FunctionDef => false
                                case _ => true
                            }
                        }
                    }
                    else true
                case _ => true
            }
        }

        def checkConditional(conditional: Conditional[(CType, DeclarationKind, Int, Linkage)],
                             recursive: Boolean = false): Boolean = {
            conditional match {
                case c@Choice(feature, then, elseB) =>
                    checkConditional(then, recursive) || checkConditional(elseB, recursive)
                case o@One((_)) => checkOne(conditional, recursive)
            }
        }

        checkConditional(env.varEnv.lookup(id.name))

    }

    private def isValidFDef(fDef: Opt[FunctionDef], call: Opt[_], ccStatement: CompoundStatement,
                            morpheus: Morpheus): Boolean = {
        if (!(fDef.feature.equivalentTo(FeatureExprFactory.True)
            || fDef.feature.implies(call.feature).isTautology(morpheus.getFM)))
            return false

        // stmt's feature does not imply fDef's feature -> no need to inline this def at this position
        assert(!isRecursive(fDef.entry), "Can not inline - method is recursive.")
        assert(!badReturnStatement(fDef.entry, morpheus), "Can not inline - method has bad return statements")
        true
    }

    private def getIdsToRename(funcDef: FunctionDef, call: AST, compoundStmt: CompoundStatement, morpheus: Morpheus) = filterAllASTElems[Id](funcDef).filter(id => isDeclared(id, morpheus.getEnv(compoundStmt.innerStatements.last.entry).asInstanceOf[Env], compoundStmt: CompoundStatement, morpheus: Morpheus))

    private def getInitializers(call: Opt[AST], parameters: List[Opt[DeclaratorExtension]],
                                   morpheus: Morpheus):
    List[Opt[DeclarationStatement]] = {

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
            val feature = morpheus.getASTEnv.featureExpr(statement).and(call.feature)

            feature.isSatisfiable(morpheus.getFM) match {
                case true => Some(statement.copy(feature = feature))
                case _ => None
            }
        })
    }

    private def getReturnStmts(statements: List[Opt[Statement]]): List[Opt[ReturnStatement]] = {
        filterAllOptElems(statements).flatMap(opt => {
            opt.entry match {
                case r: ReturnStatement => Some(opt.asInstanceOf[Opt[ReturnStatement]])
                case _ => None
            }
        })
    }
}