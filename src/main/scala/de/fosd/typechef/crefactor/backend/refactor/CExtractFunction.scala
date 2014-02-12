package de.fosd.typechef.crefactor.backend.refactor

import de.fosd.typechef.crefactor.backend.ASTSelection
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.frontend.util.Selection
import java.util
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import util.{IdentityHashMap, Collections}
import de.fosd.typechef.typesystem._
import de.fosd.typechef.parser.c.PostfixExpr
import de.fosd.typechef.parser.c.ReturnStatement
import de.fosd.typechef.parser.c.SwitchStatement
import de.fosd.typechef.parser.c.AtomicNamedDeclarator
import de.fosd.typechef.parser.c.InlineSpecifier
import de.fosd.typechef.parser.c.VolatileSpecifier
import scala.Some
import de.fosd.typechef.parser.c.DoStatement
import de.fosd.typechef.parser.c.ExternSpecifier
import de.fosd.typechef.parser.c.PointerCreationExpr
import de.fosd.typechef.parser.c.VoidSpecifier
import de.fosd.typechef.parser.c.FunctionCall
import de.fosd.typechef.conditional.{Choice, One, Opt}
import de.fosd.typechef.parser.c.RestrictSpecifier
import de.fosd.typechef.parser.c.ForStatement
import de.fosd.typechef.parser.c.DeclParameterDeclList
import de.fosd.typechef.parser.c.WhileStatement
import de.fosd.typechef.parser.c.Pointer
import de.fosd.typechef.parser.c.Declaration
import de.fosd.typechef.parser.c.ExprStatement
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.AutoSpecifier
import de.fosd.typechef.parser.c.PointerDerefExpr
import de.fosd.typechef.parser.c.GotoStatement
import de.fosd.typechef.parser.c.ExprList
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.NestedFunctionDef
import de.fosd.typechef.parser.c.BreakStatement
import de.fosd.typechef.parser.c.ContinueStatement
import de.fosd.typechef.parser.c.ParameterDeclarationD
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.parser.c.CaseStatement
import de.fosd.typechef.parser.c.RegisterSpecifier
import de.fosd.typechef.parser.c.StaticSpecifier
import de.fosd.typechef.parser.c.ConstSpecifier
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.StatsJar
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crewrite.IntraCFG


/**
 * Implements the strategy of extracting a function.
 */
// TODO Match with linking
object CExtractFunction extends ASTSelection with CRefactor with IntraCFG {

    private var lastSelection: Selection = null

    private var cachedSelectedElements: List[AST] = null

    def getSelectedElements(morpheus: Morpheus, selection: Selection): List[AST] = {
        if (lastSelection.eq(selection)) return cachedSelectedElements
        lastSelection = selection

        // TODO Better solution for Control Statements
        val ids = filterASTElementsForFile[Id](filterASTElems[Id](morpheus.getTranslationUnit).par.filter(x => isPartOfSelection(x, selection)).toList, selection.getFilePath)

        /** def findMostUpwardExpr(element: Expr): Expr = {
            parentAST(element, morpheus.getASTEnv) match {
                case e: Id => findMostUpwardExpr(e)
                case e: Constant => findMostUpwardExpr(e)
                case e: StringLit => findMostUpwardExpr(e)
                case e: UnaryExpr => findMostUpwardExpr(e)
                case e: PostfixExpr => findMostUpwardExpr(e)
                case e: SizeOfExprT => findMostUpwardExpr(e)
                case e: SizeOfExprU => findMostUpwardExpr(e)
                case e: CastExpr => findMostUpwardExpr(e)
                case e: PointerDerefExpr => findMostUpwardExpr(e)
                case e: PointerCreationExpr => findMostUpwardExpr(e)
                case e: UnaryOpExpr => findMostUpwardExpr(e)
                case e: NAryExpr => findMostUpwardExpr(e)
                case e: ExprList => findMostUpwardExpr(e)
                case e: ConditionalExpr => findMostUpwardExpr(e)
                case e: AssignExpr => findMostUpwardExpr(e)
                case x =>
                    println(x)
                    element
            }
        }   */

        def findParent(id: Id) = findPriorASTElem[Statement](id, morpheus.getASTEnv)
        // TODO Old code - trying to identify expression for extraction.
        /** priorElement match {
                case None => null
                case _ => priorElement.get match {
                    case ifState: IfStatement => Some(findMostUpwardExpr(id))
                    case elIf: ElifStatement => Some(findMostUpwardExpr(id))
                    case doState: DoStatement => Some(findMostUpwardExpr(id))
                    case whileState: WhileStatement => Some(findMostUpwardExpr(id))
                    case forState: ForStatement => Some(findMostUpwardExpr(id))
                    case returnState: ReturnStatement => Some(findMostUpwardExpr(id))
                    case switch: SwitchStatement => Some(findMostUpwardExpr(id))
                    case caseState: CaseStatement => Some(findMostUpwardExpr(id))
                    case s => Some(s)
                }
            } **/

        def exploitStatements(statement: Statement): Statement = {
            try {
                parentAST(statement, morpheus.getASTEnv) match {
                    case null => throw new RefactorException("An error during determine the preconditions occured.")
                    case f: FunctionDef => statement
                    case nf: NestedFunctionDef => statement
                    case p =>
                        if (isElementOfSelectionRange(p, selection)) {
                            exploitStatements(p.asInstanceOf[Statement])
                        } else statement
                }
            } catch {
                case _: Throwable => statement
            }
        }

        def lookupControlStatements(statement: Statement): Statement = {
            try {
                nextAST(statement, morpheus.getASTEnv) match {
                    case null => statement
                    case c: ContinueStatement =>
                        if (isElementOfSelectionRange(c, selection)) c
                        else statement
                    case b: BreakStatement =>
                        if (isElementOfSelectionRange(b, selection)) b
                        else statement
                    case c: CaseStatement =>
                        if (isElementOfSelectionRange(c, selection)) c
                        else statement
                    case g: GotoStatement =>
                        if (isElementOfSelectionRange(g, selection)) g
                        else statement
                    case r: ReturnStatement =>
                        if (isElementOfSelectionRange(r, selection)) r
                        else statement
                    case _ => statement
                }
            } catch {
                case _: Throwable => statement
            }
        }
        val uniqueSelectedStatements = Collections.newSetFromMap[Statement](new util.IdentityHashMap())
        val uniqueSelectedExpressions = Collections.newSetFromMap[Expr](new util.IdentityHashMap())

        ids.foreach(id => {
            val parent = findParent(id)
            parent match {
                case null =>
                case s: Some[Statement] =>
                    uniqueSelectedStatements.add(s.get)
                    uniqueSelectedStatements.add(lookupControlStatements(s.get))
                case x => logger.info("There might have been an expression! " + x)
            }
        })

        var parents: List[AST] = List()
        // TODO Optimize expensive array and list conversions
        if (!uniqueSelectedStatements.isEmpty) {
            parents = uniqueSelectedStatements.toArray(Array[Statement]()).toList
            uniqueSelectedStatements.clear()
            parents.foreach(statement => {
                val exploitedStatement = exploitStatements(statement.asInstanceOf[Statement])
                uniqueSelectedStatements.add(exploitedStatement)
            })
            parents = uniqueSelectedStatements.toArray(Array[Statement]()).toList
        } else parents = uniqueSelectedExpressions.toArray(Array[Expr]()).toList

        cachedSelectedElements = parents.sortWith(comparePosition)
        logger.info("ExtractFuncSelection: " + cachedSelectedElements)
        cachedSelectedElements
    }

    def getAvailableIdentifiers(morpheus: Morpheus, selection: Selection): List[Id] = getSelectedElements(morpheus, selection).isEmpty match {
        case true => null
        case false => List[Id]() // returns a empty list to signalize a valid selection was found
    }

    def isAvailable(morpheus: Morpheus, selection: List[AST]): Boolean = {
        if (selection.isEmpty) false
        else if (!selection.par.forall {
            element => findPriorASTElem[FunctionDef](element, morpheus.getASTEnv).isDefined
        }) false
        else if (!isPartOfSameCompStmt(selection, morpheus)) false
        else if (!filterAllASTElems[ReturnStatement](selection).isEmpty) false
        else if (!selection.par.forall(!isBadExtractStatement(_, selection, morpheus))) false
        else if (hasVarsToDefinedExternal(selection, morpheus)) false
        // else if (!isConditionalComplete(selection, getParentFunction(selection, morpheus), morpheus)) false // Not Relevant?
        else true
    }

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean = isAvailable(morpheus, getSelectedElements(morpheus, selection))

    def extract(morpheus: Morpheus, selection: List[AST], funName: String):
    Either[String, TranslationUnit] = {

        if (!isValidId(funName))
            return Left(Configuration.getInstance().getConfig("refactor.extractFunction.failed.shadowing"))

        // we check binding and visibility using the last element in the translation unit
        if (isShadowed(funName, morpheus.getTranslationUnit.defs.last.entry, morpheus))
            return Left(Configuration.getInstance().getConfig("default.error.invalidName"))

        // we can only handle statements. report error otherwise.
        if (selection.exists {
            case _: Expr => true
            case _ => false
        } )
            return Left("This refactoring is not yet supported!")

        if (!selection.forall {
            case _: Statement => true
            case _ => false
        } )
            return Left("Fatal error in selected elements!")

        extractStatements(morpheus, selection, funName)
    }

    private def extractStatements(morpheus: Morpheus, selection: List[AST], funcName: String):
    Either[String, TranslationUnit] = {
        try {
            val parentFunction = getParentFunction(selection, morpheus)
            val parentFunctionOpt: Opt[FunctionDef] = parentOpt(parentFunction, morpheus.getASTEnv).asInstanceOf[Opt[FunctionDef]]
            val selectedOptStatements: List[Opt[Statement]] = selection.map(selected => parentOpt(selected, morpheus.getASTEnv)).asInstanceOf[List[Opt[Statement]]]
            val selectedIds = filterAllASTElems[Id](selection)
            val compStmt = getCompoundStatement(selectedOptStatements.head.entry, morpheus)

            logger.debug(selectedOptStatements)

            /**
             * Liveness analysis
             */
            val startTime = new StopClock

            val externalUses = externalOccurrences(selectedIds, morpheus.getDeclUseMap, morpheus)
            val externalDefs = externalOccurrences(selectedIds, morpheus.getUseDeclMap, morpheus)
            val allExtRefIds = externalDefs.flatMap(x => Some(x._1))
            val extRefIds = uniqueExtRefIds(externalDefs, externalUses)
            val toDeclare = getIdsToDeclare(externalUses)

            // externalUses of selected Decls are currently refused
            if (!toDeclare.isEmpty) return Left("Invalid selection, a declared variable in the selection gets used outside.")
            val parameters = retrieveParameters(extRefIds, morpheus)
            val paramIds = getParamterIds(parameters)

            StatsJar.addStat(morpheus.getFile, Liveness, startTime.getTime)
            StatsJar.addStat(morpheus.getFile, ExternalUses, externalUses)
            StatsJar.addStat(morpheus.getFile, ExternalDecls, externalDefs)
            StatsJar.addStat(morpheus.getFile, Parameters, paramIds)

            val specifiers = generateSpecifiers(parentFunction, morpheus)
            val parameterDecls = getParameterDecls(parameters, parentFunction, morpheus)
            val declarator = generateDeclarator(funcName, parameterDecls)
            val compundStatement = generateCompoundStatement(selectedOptStatements, allExtRefIds, paramIds, morpheus)
            val newFunc = generateFuncDef(specifiers, declarator, compundStatement)
            val funcOpt = generateFuncOpt(parentFunction, newFunc, morpheus)

            val callParameters = generateFuncCallParameter(parameters)
            val functionCall = Opt[ExprStatement](funcOpt.feature, ExprStatement(PostfixExpr(Id(funcOpt.entry.getName), FunctionCall(ExprList(callParameters)))))

            // Keep changes at the AST as local as possible
            val insertedCall = insertBefore(compStmt.innerStatements, selectedOptStatements.head, functionCall)
            val ccStmtWithRemovedStmts = eqRemove(insertedCall, selectedOptStatements)
            val astWFunc = insertInAstBefore(morpheus.getTranslationUnit, parentFunctionOpt, funcOpt)

            val refAST = replaceCompoundStmt(astWFunc, compStmt, ccStmtWithRemovedStmts)
            Right(refAST)
        } catch {
            case r: RefactorException => Left(r.error)
            case x: Throwable => {
                x.printStackTrace()
                Left(x.getMessage)
            }
        }
    }

    private def hasVarsToDefinedExternal(selection: List[AST], morpheus: Morpheus): Boolean = {
        val selectedIds = filterAllASTElems[Id](selection)
        val externalUses = externalOccurrences(selectedIds, morpheus.getDeclUseMap, morpheus)
        val idsToDeclare = getIdsToDeclare(externalUses)

        if (!idsToDeclare.isEmpty) logger.error("Invalid selection for: " + selection + " with follwoing ids to declare: " + idsToDeclare)

        !idsToDeclare.isEmpty
    }

    private def getParamterIds(parameters: List[(Opt[ParameterDeclaration], Opt[Expr], Id)]) = parameters.flatMap(entry => Some(entry._3))

    private def getParameterDecls(parameters: List[(Opt[ParameterDeclaration], Opt[Expr], Id)], funcDef: FunctionDef, morpheus: Morpheus) = {
        val decls = parameters.flatMap(entry => Some(entry._1))
        List[Opt[DeclaratorExtension]](Opt(parentOpt(funcDef, morpheus.getASTEnv).feature, DeclParameterDeclList(decls)))
    }

    private def retrieveParameters(liveParamIds: List[Id], morpheus: Morpheus): List[(Opt[ParameterDeclaration], Opt[Expr], Id)] = {
        val declIdMap: IdentityHashMap[Declaration, Id] = new IdentityHashMap
        val declFeatureMap: IdentityHashMap[Declaration, FeatureExpr] = new IdentityHashMap
        val declDeclPointerMap: IdentityHashMap[Declaration, Declarator] = new IdentityHashMap
        def addTodeclIdMapMap(decl: Declaration, id: Id) = if (!declIdMap.containsKey(decl)) declIdMap.put(decl, id)
        def addToDeclFeatureMap(decl: Declaration, declFeature: FeatureExpr) = if (declFeatureMap.containsKey(decl)) declFeatureMap.put(decl, declFeature.and(declFeatureMap.get(decl))) else declFeatureMap.put(decl, declFeature)
        def addTodeclDeclPointerMap(decl: Declaration, declarator: Declarator) = if (!declDeclPointerMap.containsKey(decl)) declDeclPointerMap.put(decl, declarator)

        /**
         * Adds usual decls to possible parameters
         */
        def addDeclToDeclsToGenerate(feature: FeatureExpr, decl: Declaration, id: Id): Any = {
            addToDeclFeatureMap(decl, feature)
            addTodeclDeclPointerMap(decl, generateInit(decl, id))
            addTodeclIdMapMap(decl, id)
        }

        /**
         * Generates the init declaration for variables declared in the method body.
         */
        def generateInit(decl: Declaration, param: Id, noPointer: Boolean = false): Declarator = {
            def genPointer(entries: (List[Opt[Pointer]], List[FeatureExpr]), declSpec: Opt[Specifier]) = {
                val feature = declSpec.feature
                var addedFeatures = entries._2
                var pointers = entries._1

                if (addedFeatures.exists(ft => {
                    if (ft.equivalentTo(feature)) true
                    else if (feature.implies(ft).isTautology()) true
                    else if (ft.implies(feature).isTautology()) {
                        // Remove implied ft pointer.
                        addedFeatures = addedFeatures.diff(List(ft))
                        pointers = pointers.diff(List(Opt(ft, Pointer(List[Opt[Specifier]]()))))
                        false
                    }
                    else false
                })) entries
                else (Opt(feature, Pointer(List[Opt[Specifier]]())) :: pointers, feature :: addedFeatures)

            }

            // make pointer
            val genPointers =
                if (noPointer) List[Opt[Pointer]]()
                else decl.declSpecs.foldLeft((List[Opt[Pointer]](), List[FeatureExpr]()))((entries, declSpec) => genPointer(entries, declSpec))._1

            val resPointers = decl.init.foldLeft(genPointers)((currentPointers, declInit) => declInit.entry.declarator.pointers ::: currentPointers)

            //if (array) AtomicNamedDeclarator(pointer, Id(param.name), List[Opt[DeclaratorExtension]](Opt(FeatureExprFactory.True, DeclArrayAccess(None))))
            AtomicNamedDeclarator(resPointers, Id(param.name), List[Opt[DeclaratorExtension]]())
        }

        def addChoice(c: Choice[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True): Unit = {
            c match {
                case c@Choice(cft, o1@One(_), o2@One(_)) =>
                    addOne(o1, id)
                    addOne(o2, id /*, cft.not()*/)
                case c@Choice(cft, c1@Choice(_, _, _), o2@One(_)) =>
                    addChoice(c1, id)
                    addOne(o2, id)
                case c@Choice(cft, o1@One(_), c1@Choice(_, _, _)) =>
                    addChoice(c1, id)
                    addOne(o1, id)
                case c@Choice(cft, c1@Choice(_, _, _), c2@Choice(_, _, _)) =>
                    addChoice(c1, id)
                    addChoice(c2, id)
            }
        }

        def addOne(o: One[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True) = {
            o match {
                // only variables are interesting
                case o@One((CUnknown(_), _, _)) =>
                case o@One((CFunction(_, _), _, _)) =>
                case o@One((_, KEnumVar, _, _)) =>
                    // direct enum use -> check for visibility only as enums are constant - if not visible afterwards the refactoring can not be made.
                    if (morpheus.getUseDeclMap.get(id).exists(t => findPriorASTElem[CompoundStatement](t, morpheus.getASTEnv) match {
                        case None => false
                        case _ => true
                    })) throw new RefactorException("Type Declaration for " + id.name + " would be invisible after extraction!")
                case o@One((CType(_, _, _, _), KParameter, _, _)) =>
                    // Passed as parameter in parent function
                    val decl = findPriorASTElem[ParameterDeclaration](id, morpheus.getASTEnv)
                    decl match {
                        case Some(p@ParameterDeclarationD(_, _, _)) =>
                            val genDeclFromPDecl = Declaration(p.specifiers, List(Opt(ft, InitDeclaratorI(p.decl, p.attr, None))))
                            addDeclToDeclsToGenerate(ft, genDeclFromPDecl, id)
                        case x => logger.error("Missed parameter decl pattern" + x)
                    }
                case _ =>
                    // Declared in parent function or globally definied
                    val decl = findPriorASTElem[Declaration](id, morpheus.getASTEnv)
                    decl match {
                        case Some(entry) => {
                            val feature = if (ft.equivalentTo(FeatureExprFactory.True)) parentOpt(entry, morpheus.getASTEnv).feature else ft
                            // TODO Possible Scala Bug?
                            // addDeclToDeclsToGenerate(feature, entry, id)
                            addToDeclFeatureMap(entry, feature)
                            addTodeclDeclPointerMap(entry, generateInit(entry, id))
                            addTodeclIdMapMap(entry, id)
                        }
                        case x => logger.error("Missed declaration: " + x)
                    }
            }
        }

        liveParamIds.foreach(liveId =>
            try {
                // only lookup variables
                morpheus.getEnv(liveId).varEnv.lookup(liveId.name) match {
                    case o@One(_) => addOne(o, liveId)
                    case c@Choice(_, _, _) => addChoice(c, liveId)
                    case x => logger.warn("Missed pattern choice? " + x)
                }
            } catch {
                case _: Throwable => // logger.warn("No entry found for: " + param)
            })

        val decls = declFeatureMap.keySet().toArray(Array[Declaration]()).toList
        decls.flatMap(decl => {
            val feature = decls.foldLeft(declFeatureMap.get(decl))(f = (ft, otherDecl) => {
                if (declDeclPointerMap.get(decl).getName.equals(declDeclPointerMap.get(otherDecl).getName) && !decl.eq(otherDecl)) {
                    val andFeature = declFeatureMap.get(otherDecl).not()
                    if (!andFeature.equivalentTo(FeatureExprFactory.False)) ft.and(andFeature)
                    else ft
                } else ft
            })

            decl.declSpecs.foreach(spec => {
                spec.entry match {
                    case t@TypeDefTypeSpecifier(i@Id(_)) =>
                        if (morpheus.getUseDeclMap.get(i).exists(t => findPriorASTElem[CompoundStatement](t, morpheus.getASTEnv) match {
                            case None => false
                            case _ => true
                        })) throw new RefactorException("Type Declaration for " + i + " would be invisible after extraction!")
                    case s@StructOrUnionSpecifier(_, Some(i@Id(_)), _, _, _) =>
                        if (morpheus.getUseDeclMap.get(i).exists(t => findPriorASTElem[CompoundStatement](t, morpheus.getASTEnv) match {
                            case None => false
                            case _ => true
                        })) throw new RefactorException("Type Declaration for " + i + " would be invisible after extraction!")
                    case _ => logger.debug("Specs " + spec)
                }
            })

            // remove extern specifier in function argument.
            val filteredDeclSpecs = decl.declSpecs.filter(_.entry match {
                case e: ExternSpecifier => false
                case _ => true
            })

            val paramDecl = Opt(feature, ParameterDeclarationD(filteredDeclSpecs, declDeclPointerMap.get(decl), List()))
            val expr = Opt(feature, PointerCreationExpr(Id(declDeclPointerMap.get(decl).getName)))
            val id = declIdMap.get(decl)
            Some((paramDecl, expr, id))
        })
    }

    private def isPartOfSameCompStmt(selection: List[AST], morpheus: Morpheus): Boolean =
        findPriorASTElem[CompoundStatement](selection.head, morpheus.getASTEnv) match {
            case Some(c) => selection.par.forall(element => isElementOfEqCompStmt(element, c, morpheus))
            case _ => false // not element of an ccStmt
        }

    /**
     * Generates the parameters requiered in the function stmt.
     */
    private def generateFuncCallParameter(parameters: List[(Opt[ParameterDeclaration], Opt[Expr], Id)]) = parameters.flatMap(entry => Some(entry._2))


    private def uniqueExtRefIds(defs: List[(Id, List[Id])], uses: List[(Id, List[Id])]) = {
        val parameterIds = Collections.newSetFromMap[Id](new util.IdentityHashMap())
        defs.foreach(x => x._2.foreach(entry => parameterIds.add(entry)))
        uses.foreach(x => parameterIds.add(x._1))
        parameterIds.toArray(Array[Id]()).toList.sortWith(compareByName)
    }

    private def getIdsToDeclare(uses: List[(Id, List[Id])]) = {
        val declarationIds = Collections.newSetFromMap[Id](new util.IdentityHashMap())
        uses.foreach(id => declarationIds.add(id._1))
        declarationIds.toArray(Array[Id]()).toList.sortWith(compareByName)
    }


    private def generateCompoundStatement(statements: List[Opt[Statement]], externalRef: List[Id], parameters: List[Id], morpheus: Morpheus): CompoundStatement = {
        def isPartOfParameter(id: Id, params: List[Id], morpheus: Morpheus): Boolean = {
            if (!morpheus.getUseDeclMap.containsKey(id)) false
            morpheus.getUseDeclMap.get(id).exists(decl => params.exists(param => param.eq(decl)))
        }

        val variables = externalRef.par.flatMap(id => isPartOfParameter(id, parameters, morpheus) match {
            case true => Some(id)
            case _ => None
        }).toList

        // Make Pointer
        val idsAsPointer = variables.foldLeft(statements)((stmts, id) => replaceInAST(stmts, id, PointerDerefExpr(id)))
        CompoundStatement(idsAsPointer)
    }


    private def isPartOfFuncCall(id: Id, morpheus: Morpheus): Boolean = {
        morpheus.getASTEnv.parent(id) match {
            case PostfixExpr(`id`, FunctionCall(_)) => true
            case _ => false
        }
    }

    private def externalOccurrences(ids: List[Id], map: IdentityIdHashMap, morpheus: Morpheus) =
        ids.par.flatMap(id => {
            if (map.containsKey(id) && !isPartOfFuncCall(id, morpheus)) {
                val external = map.get(id).par.flatMap(aId => {
                    if (ids.par.exists(oId => oId.eq(aId))) None
                    else Some(aId)
                }).toList
                if (external.isEmpty) None
                else Some(id, external)
            } else None
        }).toList

    private def isElementOfEqCompStmt(element: AST, compStmt: CompoundStatement, morpheus: Morpheus) = getCompoundStatement(element, morpheus).eq(compStmt)

    private def getCompoundStatement(element: AST, morpheus: Morpheus): CompoundStatement =
        findPriorASTElem[CompoundStatement](element, morpheus.getASTEnv) match {
            case Some(c) => c
            case _ => null
        }

    /**
     * Conditional complete?
     */
    /*
    private def isConditionalComplete(selection: List[AST], parentFunction: FunctionDef, morpheus: Morpheus): Boolean = {
        if (selection.isEmpty) return false

        if (!selectionIsConditional(selection)) return true
        // no variable configuration -> conditional complete

        if (!(filterAllFeatureExpr(selection).toSet.size > 1)) return true
        // only one and the same feature -> conditonal complete
        val expr1 = selection.head.asInstanceOf[Opt[_]]
        val expr2 = selection.last.asInstanceOf[Opt[_]]

        if (expr1.feature.equivalentTo(expr2.feature)) return true
        // start and end feature are the same -> eligable
        val prevState = prevOpt(expr1, morpheus.getASTEnv)
        val nextState = nextOpt(expr2, morpheus.getASTEnv)

        if (((prevState != null) && prevState.feature.equals(expr2.feature))
            || ((nextState != null) && nextState.feature.equals(expr1.feature))
            || ((prevState != null) && (nextState != null) && nextState.feature.equals(prevState.feature))) return true
        // prev feature and next feature are the same -> eligible
        // TODO Null States!
        false
    }
    */

    /**
     * InlineFuncOptionSelector is conditonal?
     */
    /*
    private def selectionIsConditional(selection: List[AST]) = selection.exists(isVariable)
    */

    private def isBadExtractStatement(element: AST, selection: List[AST], morpheus: Morpheus): Boolean = {

        def filter[T <: AST](stmts: List[AST])(implicit m: ClassManifest[T]) = {
            stmts.exists(stmt => {
                findPriorASTElem[T](stmt, morpheus.getASTEnv) match {
                    case None => false
                    case Some(x) =>
                        selection.exists(s =>
                            if (s.eq(x)) true
                            else filterAllASTElems[T](s, morpheus.getASTEnv).par.exists(fs => fs.eq(x)))
                }
            })
        }

        val cStmt = filterAllASTElems[ContinueStatement](element)
        cStmt.isEmpty match {
            case true =>
            case _ =>
                if (filter[ForStatement](cStmt)) return false
                if (filter[DoStatement](cStmt)) return false
                if (filter[WhileStatement](cStmt)) return false
                return true
        }
        val bStmt = filterAllASTElems[BreakStatement](element)
        bStmt.isEmpty match {
            case true =>
            case _ =>
                if (filter[DoStatement](bStmt)) return false
                if (filter[WhileStatement](bStmt)) return false
                if (filter[ForStatement](bStmt)) return false
                if (filter[SwitchStatement](bStmt)) return false
                return true
        }
        val caStmt = filterAllASTElems[CaseStatement](element)
        caStmt.isEmpty match {
            case true =>
            case _ =>
                if (filter[SwitchStatement](caStmt)) return false
                return true
        }
        val gotoS = filterAllASTElems[GotoStatement](element)
        gotoS.isEmpty match {
            case true =>
            case _ => return !gotoS.exists(goto => {
                goto.target match {
                    case i: Id => morpheus.getUseDeclMap.get(i).exists(labels => filter[Id](gotoS))
                    case _ => true
                }
            })
        }
        val labels = filterAllASTElems[LabelStatement](element)
        labels.isEmpty match {
            case true =>
            case _ => return !labels.exists(label => morpheus.getDeclUseMap.get(label.id).exists(goto => filter[Id](labels)))
        }
        false
    }

    private def getParentFunction(selection: List[AST], morpheus: Morpheus): FunctionDef = {
        findPriorASTElem[FunctionDef](selection.head, morpheus.getASTEnv) match {
            case Some(f) => f
            case _ => null
        }
    }

    /**
     * Generates the required specifiers.
     */
    private def generateSpecifiers(funcDef: FunctionDef, morpheus: Morpheus /* , typeSpecifier: Opt[Specifier] = Opt(FeatureExprFactory.True, VoidSpecifier()) */): List[Opt[Specifier]] = {
        var specifiers: List[Opt[Specifier]] = List(Opt(parentOpt(funcDef, morpheus.getASTEnv).feature, VoidSpecifier()))

        // preserv specifiers from function definition except type specifiers
        funcDef.specifiers.foreach(specifier => {
            specifier.entry match {
                case InlineSpecifier() => specifiers ::= specifier
                case AutoSpecifier() => specifiers ::= specifier
                case RegisterSpecifier() => specifiers ::= specifier
                case VolatileSpecifier() => specifiers ::= specifier
                case ExternSpecifier() => specifiers ::= specifier
                case ConstSpecifier() => specifiers ::= specifier
                case RestrictSpecifier() => specifiers ::= specifier
                case StaticSpecifier() => specifiers ::= specifier
                case _ =>
            }
        })
        specifiers
    }

    /**
     * Generates the function definition.
     */
    private def generateFuncDef(specs: List[Opt[Specifier]], decl: Declarator, stmts: CompoundStatement, oldStyleParameters: List[Opt[OldParameterDeclaration]] = List[Opt[OldParameterDeclaration]]()) = FunctionDef(specs, decl, oldStyleParameters, stmts)

    /**
     * Generates the opt node for the tunit.
     */
    private def generateFuncOpt(oldFunc: FunctionDef, newFunc: FunctionDef, morpheus: Morpheus, feature: FeatureExpr = FeatureExprFactory.True) = Opt[FunctionDef](parentOpt(oldFunc, morpheus.getASTEnv).feature.and(feature), newFunc)

    /**
     * Generates the decl.
     */
    private def generateDeclarator(name: String /*, pointer: List[Opt[Pointer]] = List[Opt[Pointer]]()*/ , extensions: List[Opt[DeclaratorExtension]] = List[Opt[DeclaratorExtension]]()) = AtomicNamedDeclarator(List[Opt[Pointer]](), Id(name), extensions)
}