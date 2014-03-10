package de.fosd.typechef.crefactor.backend.engine

import java.util.Collections

import de.fosd.typechef.crefactor.backend.{RefactorException, CRefactor, ASTSelection}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.frontend.util.Selection
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import de.fosd.typechef.typesystem._
import de.fosd.typechef.conditional.{ConditionalLib, Choice, One, Opt}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.StatsCan
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crewrite.IntraCFG


/**
 * Implements the strategy of extracting a function.
 */
object CExtractFunction extends ASTSelection with CRefactor with IntraCFG {

    private var lastSelection: Selection = null

    private var cachedSelectedElements: List[AST] = null

    def getSelectedElements(morpheus: Morpheus, selection: Selection): List[AST] = {
        if (lastSelection.eq(selection))
            return cachedSelectedElements

        lastSelection = selection
        val ids = filterASTElementsForFile[Id](
            filterASTElems[Id](morpheus.getTranslationUnit).par.filter(x => isPartOfSelection(x, selection)).toList, selection.getFilePath)

        // TODO: @andreas What is this function doing?
        // @JL: this function tries to find the greatest statement of a selection: for example:
        // if (1) {
        //     i++;
        // }
        // in case the whole if statement is selected we don't want to add the i++ statement to our selection list,
        // as it is already part of the if statement
        def exploitStatements(statement: Statement): Statement = {
            try {
                parentAST(statement, morpheus.getASTEnv) match {
                    // TODO: @andreas Suggestion for a better error message. "No proper selection for extract function."
                    case null => throw new RefactorException("An error occurred during determining preconditions.")
                    case _: FunctionDef => statement
                    case _: NestedFunctionDef => statement
                    case p =>
                        if (isElementOfSelectionRange(p, selection)) {
                            exploitStatements(p.asInstanceOf[Statement])
                        } else
                            statement
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
        val uniqueSelectedStatements = Collections.newSetFromMap[Statement](new java.util.IdentityHashMap())
        val uniqueSelectedExpressions = Collections.newSetFromMap[Expr](new java.util.IdentityHashMap())

        ids.foreach(id => {
            val parent = findPriorASTElem[Statement](id, morpheus.getASTEnv)
            parent match {
                case null =>
                case s: Some[Statement] =>
                    uniqueSelectedStatements.add(s.get)
                    uniqueSelectedStatements.add(lookupControlStatements(s.get))
                case x => logger.info("There may have been an expression! " + x)
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
        } else
            parents = uniqueSelectedExpressions.toArray(Array[Expr]()).toList

        cachedSelectedElements = parents.sortWith(comparePosition)
        logger.info("ExtractFuncSelection: " + cachedSelectedElements)
        cachedSelectedElements
    }

    def getAvailableIdentifiers(morpheus: Morpheus, selection: Selection): List[Id] =
        getSelectedElements(morpheus, selection).isEmpty match {
            case true => null
            case false => List[Id]() // returns a empty list to signalize a valid selection was found
        }

    def isAvailable(morpheus: Morpheus, selection: List[AST]): Boolean = {
        if (selection.isEmpty)
            false
        else if (!selection.par.forall {
            element => findPriorASTElem[FunctionDef](element, morpheus.getASTEnv).isDefined
        }) false
        else if (!isPartOfSameCompStmt(selection, morpheus)) false
        else if (!filterAllASTElems[ReturnStatement](selection).isEmpty) false
        else if (!selection.par.forall(checkAstElemForCFGDisruption(_, selection, morpheus))) false
        else if (hasIdsWithDifferentScope(selection, morpheus)) false
        else if (hasInvisibleEnumerations(selection, morpheus)) false
        // else if (!isConditionalComplete(selection, getParentFunction(selection, morpheus), morpheus)) false // Not Relevant?
        else true
    }

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean =
        isAvailable(morpheus, getSelectedElements(morpheus, selection))

    def extract(morpheus: Morpheus, selection: List[AST], funName: String):
    Either[String, TranslationUnit] = {

        if (!isValidId(funName))
            return Left(Configuration.getInstance().getConfig("default.error.invalidName"))

        // Reference check is performed as soon as we know the featureExpr the new function is going to have!

        val oldFDef = findPriorASTElem[FunctionDef](selection.head, morpheus.getASTEnv)
        if (isValidInProgram(Opt(morpheus.getASTEnv.featureExpr(oldFDef.get), funName), morpheus))
            return Left(Configuration.getInstance().getConfig(
                "default.error.isInConflictWithSymbolInModuleInterface"))

        // we check binding and visibility using the last element in the translation unit
        if (isValidInModule(funName, morpheus.getTranslationUnit.defs.last.entry, morpheus))
            return Left(Configuration.getInstance().getConfig("default.error.isInConflictWithSymbolInModule"))

        // we can only handle statements. report error otherwise.
        if (selection.exists {
            case _: Expr => true
            case _ => false
        })
            return Left(Configuration.getInstance().getConfig("refactor.extractFunction.failed.unsupported"))

        if (!selection.forall {
            case _: Statement => true
            case _ => false
        })
            return Left(Configuration.getInstance().getConfig("refactor.extractFunction.failed.invalidSelection"))

        extractStatements(morpheus, selection, funName)
    }

    private def extractStatements(morpheus: Morpheus, selection: List[AST], funcName: String):
    Either[String, TranslationUnit] = {
        try {
            val parentFunction = getFunction(selection, morpheus)
            val parentFunctionOpt: Opt[FunctionDef] = parentOpt(parentFunction,
                morpheus.getASTEnv).asInstanceOf[Opt[FunctionDef]]
            val selectedOptStatements: List[Opt[Statement]] =
                selection.map {
                    elem => {
                        parentOpt(elem, morpheus.getASTEnv).asInstanceOf[Opt[Statement]]
                    }
                }
            val selectedIds = filterAllASTElems[Id](selection)
            val compStmt = getCompoundStatement(selectedOptStatements.head.entry, morpheus)

            logger.debug(selectedOptStatements)

            /**
             * determine variables that are live outside of the selection and that so need to be
             * passed as parameters to the newly create function
             */
            val startTime = new StopClock

            val externalUses = externalOccurrences(selectedIds, morpheus.getDeclUseMap, morpheus)
            val externalDefs = externalOccurrences(selectedIds, morpheus.getUseDeclMap, morpheus)
            val allExtRefIds = externalDefs.flatMap(x => Some(x._1))
            val extRefIds = uniqueExtRefIds(externalDefs, externalUses)
            val toDeclare = getIdsToDeclare(externalUses)

            // usages of declared variables outside the selection is not supported yet
            // in case we reorder the elements it may be possible to apply the refactoring.
            if (!toDeclare.isEmpty)
                return Left("Invalid selection, a declared variable in the selection gets used outside.")

            val params = retrieveParameters(extRefIds, morpheus)
            val paramsIds = params.map(_._3)

            StatsCan.addStat(morpheus.getFile, Liveness, startTime.getTime)
            StatsCan.addStat(morpheus.getFile, ExternalUses, externalUses)
            StatsCan.addStat(morpheus.getFile, ExternalDecls, externalDefs)
            StatsCan.addStat(morpheus.getFile, Parameters, paramsIds)

            // generate new function definition
            val specifiers = genSpecifiers(parentFunction, morpheus)
            val parameterDecls = getParameterDecls(params, parentFunction, morpheus)
            val declarator = genDeclarator(funcName, parameterDecls)
            val compundStatement = genCompoundStatement(selectedOptStatements,
                allExtRefIds, paramsIds, morpheus)
            val newFDef = genFDef(specifiers, declarator, compundStatement)
            val newFDefOpt = genFDefExternal(parentFunction, newFDef, morpheus)

            if (isValidInProgram(Opt(newFDefOpt.feature, funcName), morpheus))
                return Left(Configuration.getInstance().getConfig("default.error.invalidName"))

            // generate function fCall
            val callParameters = genFCallParams(params)
            val functionCall = Opt[ExprStatement](newFDefOpt.feature,
                ExprStatement(PostfixExpr(Id(newFDefOpt.entry.getName),
                    FunctionCall(ExprList(callParameters)))))

            // Keep changes at the AST as local as possible
            // TODO: Check with single compound statements.
            val tunitWithFCall = insertBefore(compStmt.innerStatements,
                    selectedOptStatements.head, functionCall)
            val ccStmtWithRemovedStmts = eqRemove(tunitWithFCall, selectedOptStatements)
            val tunitWithFDef = insertInAstBefore(morpheus.getTranslationUnit,
                parentFunctionOpt, newFDefOpt)

            val refAST = replaceCompoundStmt(tunitWithFDef, compStmt, ccStmtWithRemovedStmts)
            Right(refAST)
        } catch {
            case r: RefactorException => Left(r.error)
            case x: Throwable => {
                x.printStackTrace()
                Left(x.getMessage)
            }
        }
    }

    private def hasInvisibleEnumerations(selection: List[AST], morpheus: Morpheus): Boolean = {

        val selectedIds = filterAllASTElems[Id](selection)
        val externalUses = externalOccurrences(selectedIds, morpheus.getDeclUseMap, morpheus)
        val externalDefs = externalOccurrences(selectedIds, morpheus.getUseDeclMap, morpheus)
        val liveIds = uniqueExtRefIds(externalDefs, externalUses)

        val invisibleEnums = liveIds.exists(liveId => {
            try {
                val enums = ConditionalLib.leaves(morpheus.getEnv(liveId).varEnv.lookup(liveId.name))
                val res = enums.exists { case (_, KEnumVar, 1, _) => true; case _ => false }

                if (res)
                    logger.info(liveId + " is invisible after extraction")

                res
            } catch {
                case _: Throwable =>
                    logger.warn("No entry found for: " + liveId)
                    false
            }
        })

        if (invisibleEnums)
            logger.info("Not available for extract - has invisible enums.")

        invisibleEnums
    }

    private def hasIdsWithDifferentScope(selection: List[AST], morpheus: Morpheus): Boolean = {
        val selectedIds = filterAllASTElems[Id](selection)
        val externalUses = externalOccurrences(selectedIds, morpheus.getDeclUseMap, morpheus)
        val idsToDeclare = getIdsToDeclare(externalUses)

        if (!idsToDeclare.isEmpty) logger.error("Invalid selection for: " + selection +
            " with following ids to declare: " + idsToDeclare)

        !idsToDeclare.isEmpty
    }

    private def getParameterDecls(parameters: List[(Opt[ParameterDeclaration], Opt[Expr], Id)],
                                  fDef: FunctionDef, morpheus: Morpheus) = {
        val decls = parameters.flatMap(entry => Some(entry._1))
        List[Opt[DeclaratorExtension]](Opt(parentOpt(fDef, morpheus.getASTEnv).feature,
            DeclParameterDeclList(decls)))
    }

    private def retrieveParameters(liveParamIds: List[Id], morpheus: Morpheus):
    List[(Opt[ParameterDeclaration], Opt[Expr], Id)] = {
        val declIdMap: java.util.IdentityHashMap[Declaration, Id] =
            new java.util.IdentityHashMap
        val declFeatureMap: java.util.IdentityHashMap[Declaration, FeatureExpr] =
            new java.util.IdentityHashMap
        val declDeclPointerMap: java.util.IdentityHashMap[Declaration, Declarator] =
            new java.util.IdentityHashMap

        def addTodeclIdMapMap(decl: Declaration, id: Id) =
            if (!declIdMap.containsKey(decl)) declIdMap.put(decl, id)

        def addToDeclFeatureMap(decl: Declaration, declFeature: FeatureExpr) =
            if (declFeatureMap.containsKey(decl))
                declFeatureMap.put(decl, declFeature.and(declFeatureMap.get(decl)))
            else declFeatureMap.put(decl, declFeature)

        def addTodeclDeclPointerMap(decl: Declaration, declarator: Declarator) =
            if (!declDeclPointerMap.containsKey(decl))
                declDeclPointerMap.put(decl, declarator)

        /**
         * Adds usual decls to possible parameters
         */
        def addDeclToDeclsToGenerate(feature: FeatureExpr, decl: Declaration, id: Id): Any = {
            addToDeclFeatureMap(decl, feature)
            addTodeclDeclPointerMap(decl, generateInit(decl, id))
            addTodeclIdMapMap(decl, id)
        }

        def addParameterFromParameter(id: Id, ft: FeatureExpr, featureExploit: Boolean = true) = {

            def retrieveAllDeclParameterFeatures(paramDecl: Product, feature: FeatureExpr): FeatureExpr = {
                // get up to DeclParameterDeclList to make sure ALL features are found
                val parent = parentOpt(paramDecl, morpheus.getASTEnv)
                parent.entry match {
                    case d: DeclParameterDeclList => feature.and(parent.feature)
                    case p: Product => retrieveAllDeclParameterFeatures(parent, feature.and(parent.feature))
                    case x =>
                        logger.error("Missed parent: " + x)
                        feature
                }
            }

            val decl = findPriorASTElem[ParameterDeclaration](id, morpheus.getASTEnv)
            decl match {
                case Some(p@ParameterDeclarationD(_, _, _)) =>
                    val feature = {
                        if (featureExploit) retrieveAllDeclParameterFeatures(p, ft)
                        else ft
                    }
                    val genDeclFromPDecl = Declaration(p.specifiers, List(Opt(ft, InitDeclaratorI(p.decl, p.attr, None))))
                    addDeclToDeclsToGenerate(feature, genDeclFromPDecl, id)
                case x => logger.error("Missed parameter decl pattern" + x)
            }
        }

        /**
         * Generates the init declaration for variables declared in the method body.
         */
        def generateInit(decl: Declaration, param: Id, noPointer: Boolean = false): Declarator = {

            def genPointer(entries: (List[Opt[Pointer]], List[FeatureExpr]),
                           declSpec: Opt[Specifier]) = {
                val feature = declSpec.feature
                var (pointers, addedFeatures) = entries

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
                else decl.declSpecs.foldLeft((List[Opt[Pointer]](), List[FeatureExpr]()))
                    { (entries, declSpec) => genPointer(entries, declSpec) }._1

            val resPointers = decl.init.foldLeft(genPointers)
                { (currentPointers, declInit) => declInit.entry.declarator.pointers ::: currentPointers }

            //if (array)
            // AtomicNamedDeclarator(pointer, Id(param.name),
            // List[Opt[DeclaratorExtension]](Opt(FeatureExprFactory.True, DeclArrayAccess(None))))
            AtomicNamedDeclarator(resPointers, Id(param.name), List[Opt[DeclaratorExtension]]())
        }

        def addChoice(c: Choice[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True): Unit = {
            c match {
                case c@Choice(cft, o1@One(_), o2@One(_)) =>
                    addOne(o1, id)
                    addOne(o2, id, cft.not())
                case c@Choice(cft, c1@Choice(_, _, _), o2@One(_)) =>
                    addChoice(c1, id)
                    addOne(o2, id, cft.not())
                case c@Choice(cft, o1@One(_), c1@Choice(_, _, _)) =>
                    addChoice(c1, id, cft.not())
                    addOne(o1, id)
                case c@Choice(cft, c1@Choice(_, _, _), c2@Choice(_, _, _)) =>
                    addChoice(c1, id)
                    addChoice(c2, id, cft.not())
            }
        }

        def addChoiceOne(c: Choice[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True) = {
            val orFeatures = getOrFeatures(c)
            logger.info("OneChoiceFeature " + orFeatures)
            val featureExpr = ft.and(orFeatures)
            addParameterFromDeclaration(id, featureExpr, false)
        }

        def addParameterFromDeclaration(id: Id, ft: FeatureExpr, featureExploit: Boolean = true) {
            val decl = findPriorASTElem[Declaration](id, morpheus.getASTEnv)
            decl match {
                case Some(entry) => {
                    val feature = if (ft.equivalentTo(FeatureExprFactory.True))
                                      parentOpt(entry, morpheus.getASTEnv).feature
                                  else
                                      ft
                    // TODO Possible Scala Bug?
                    // addDeclToDeclsToGenerate(feature, entry, id)
                    addToDeclFeatureMap(entry, feature)
                    addTodeclDeclPointerMap(entry, generateInit(entry, id))
                    addTodeclIdMapMap(entry, id)
                }
                case none =>
                    // fallback as parameter from parameter...
                    addParameterFromParameter(id, ft, featureExploit)
                    logger.warn("Passed as parameter and detected as declaration but not as parameter: " + id)
            }
        }

        def addOne(o: One[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True) = {
            if (ft.isTautology(morpheus.getFM)) {
                o match {
                    // only variables are interesting
                    case One((CUnknown(_), _, _)) =>
                    case One((CFunction(_, _), _, _)) =>
                    case One((CType(CFunction(_, _), _, _, _), _, _, ExternalLinkage)) =>
                    case One((CType(CFunction(_, _), _, _, _), _, _, InternalLinkage)) =>
                    case One((_, KEnumVar, _, _)) =>
                        // direct enum use -> check for visibility only as enums are constant
                        // if not visible afterwards the refactoring can not be made.
                        if (morpheus.getDecls(id).exists(t => findPriorASTElem[CompoundStatement](t, morpheus.getASTEnv) match {
                            case None => false
                            case _ => true
                        })) throw new RefactorException("Type Declaration for " + id.name +
                            " would be invisible after extraction!")
                    case One((CType(_, _, _, _), KParameter, _, _)) =>
                        // Passed as parameter in parent function
                        addParameterFromParameter(id, ft)
                    case _ =>
                        // Declared in parent function or globally defined
                        addParameterFromDeclaration(id, ft)
                    }
            }
        }

        liveParamIds.foreach(liveId =>
            try {
                // only lookup variables
                morpheus.getEnv(liveId).varEnv.lookup(liveId.name) match {
                    case o@One(_) => addOne(o, liveId)
                    case c@Choice(_, _, _) =>
                        // retrieve if choice is only a fake choice, caused by the parser code duplication
                        val singleChoice = liveParamIds.flatMap(id => {
                            if (id.name.equals(liveId.name)) Some(liveId)
                            else None
                        })
                        if (singleChoice.length < 2) addChoiceOne(c, liveId)
                        else addChoice(c, liveId)
                    case x => logger.warn("Missed pattern choice? " + x)
                }
            } catch {
                case _: Throwable => logger.warn("No entry found for: " + liveId)
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
                        if (morpheus.getDecls(i).exists(t => findPriorASTElem[CompoundStatement](t, morpheus.getASTEnv) match {
                            case None => false
                            case _ => true
                        })) throw new RefactorException("Type Declaration for " + i +
                            " would be invisible after extraction!")
                    case s@StructOrUnionSpecifier(_, Some(i@Id(_)), _, _, _) =>
                        if (morpheus.getDecls(i).exists(t => findPriorASTElem[CompoundStatement](t, morpheus.getASTEnv) match {
                            case None => false
                            case _ => true
                        })) throw new RefactorException("Type Declaration for " + i +
                            " would be invisible after extraction!")
                    case _ => logger.debug("Specs " + spec)
                }
            })

            // remove extern specifier in function argument.
            val filteredDeclSpecs = decl.declSpecs.filter(_.entry match {
                case s: OtherSpecifier => false
                case _ => true
            })

            val paramDecl = Opt(feature, ParameterDeclarationD(filteredDeclSpecs,
                declDeclPointerMap.get(decl), List()))
            val expr = Opt(feature, PointerCreationExpr(Id(declDeclPointerMap.get(decl).getName)))
            val id = declIdMap.get(decl)
            Some((paramDecl, expr, id))
        })
    }

    private def isPartOfSameCompStmt(selection: List[AST], morpheus: Morpheus): Boolean =
        findPriorASTElem[CompoundStatement](selection.head, morpheus.getASTEnv) match {
            case Some(c) => selection.par.forall(element => isElementOfEqCompStmt(element, c, morpheus))
            case _ => false // not an element of an ccStmt
        }

    /**
     * Generate the parameters required in the function stmt.
     */
    private def genFCallParams(parameters: List[(Opt[ParameterDeclaration], Opt[Expr], Id)]) =
        parameters.map(_._2)

    private def uniqueExtRefIds(defs: List[(Id, List[Id])], uses: List[(Id, List[Id])]) = {
        val parameterIds = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        defs.foreach(x => x._2.foreach(entry => parameterIds.add(entry)))
        uses.foreach(x => parameterIds.add(x._1))
        parameterIds.toArray(Array[Id]()).toList.sortWith { (i1, i2) => i1.name < i2.name }
    }

    private def getIdsToDeclare(uses: List[(Id, List[Id])]) = {
        val declarationIds = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        uses.foreach(id => declarationIds.add(id._1))
        declarationIds.toArray(Array[Id]()).toList.sortWith { (i1, i2) => i1.name < i2.name }
    }

    private def genCompoundStatement(statements: List[Opt[Statement]], externalRef: List[Id],
                                     parameters: List[Id], morpheus: Morpheus): CompoundStatement = {
        def isPartOfParameter(id: Id, morpheus: Morpheus): Boolean = {
            if (!morpheus.isInUseDeclMap(id)) false
            morpheus.getDecls(id).exists(decl => parameters.exists(param => param.eq(decl)))
        }

        val variables = externalRef.par.filter(isPartOfParameter(_, morpheus))

        // transform input ids into pointer expressions
        val idsAsPointer = variables.foldLeft(statements)((curStatements, id) =>
            replaceInAST(curStatements, id, PointerDerefExpr(id)))
        CompoundStatement(idsAsPointer)
    }

    private def isPartOfFuncCall(id: Id, morpheus: Morpheus): Boolean = {
        morpheus.getASTEnv.parent(id) match {
            case PostfixExpr(`id`, FunctionCall(_)) => true
            case _ => false
        }
    }

    // TODO Rewrite Method without old usedecl and decluse usage
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

    private def isElementOfEqCompStmt(element: AST, compStmt: CompoundStatement, morpheus: Morpheus) =
        getCompoundStatement(element, morpheus).eq(compStmt)

    private def getCompoundStatement(element: AST, morpheus: Morpheus): CompoundStatement =
        findPriorASTElem[CompoundStatement](element, morpheus.getASTEnv) match {
            case Some(c) => c
            case _ => null
        }

    private def checkAstElemForCFGDisruption(element: AST, selection: List[AST], morpheus: Morpheus): Boolean = {
        
        // determine all continue statements and check whether their jump targets
        // are part of the selection
        val cStmts = filterAllASTElems[ContinueStatement](element)
        val succsCStmts = cStmts.flatMap(succ(_, morpheus.getASTEnv))
        val filteredCStmtsSuccs = succsCStmts.filter(_.feature isSatisfiable morpheus.getFM)
        
        if (! filteredCStmtsSuccs.forall(isPartOf(_, selection)))
            return false

        // determine all break statements and check whether their jump targers
        // are part of the selection
        val bStmts = filterAllASTElems[BreakStatement](element)
        val succsBStmts = bStmts.flatMap(succ(_, morpheus.getASTEnv))
        val filteredBStmtsSuccs = succsBStmts.filter(_.feature isSatisfiable morpheus.getFM)

        if (! filteredBStmtsSuccs.par.forall(isPartOf(_, selection)))
            return false

        // determine all case statements and check whether their predecessor elements, in particular,
        // the entire switch statement is part of the selection
        val caStmts = filterAllASTElems[CaseStatement](element)
        val predsCaStmts = caStmts.flatMap(pred(_, morpheus.getASTEnv))
        val filteredCaStmts = predsCaStmts.filter(_.feature isSatisfiable morpheus.getFM)

        if (! filteredCaStmts.par.forall(isPartOf(_, selection)))
            return false

        // determine goto and label statements and check whether their references (succs resp. preds)
        // belong to the selection
        val gotoStmts = filterAllASTElems[GotoStatement](element)
        val succsGotoStmts = gotoStmts.flatMap(succ(_, morpheus.getASTEnv))
        val filteredGotoStmts = succsGotoStmts.filter(_.feature isSatisfiable morpheus.getFM)
        
        if (! filteredGotoStmts.par.forall(isPartOf(_, selection)))
            return false

        val labelStmts = filterAllASTElems[LabelStatement](element)
        val predsLabelStmts = labelStmts.flatMap(pred(_, morpheus.getASTEnv))
        val filteredLabelStmts = predsLabelStmts.filter(_.feature isSatisfiable morpheus.getFM)
        
        if (! filteredLabelStmts.par.forall(isPartOf(_, selection)))
            return false

        true
    }

    private def getFunction(selection: List[AST], morpheus: Morpheus): FunctionDef = {
        findPriorASTElem[FunctionDef](selection.head, morpheus.getASTEnv) match {
            case Some(f) => f
            case _ => null
        }
    }

    /**
     * Generates the required specifiers.
     */
    private def genSpecifiers(fDef: FunctionDef, morpheus: Morpheus): List[Opt[Specifier]] = {
        // preserve specifiers from function definition except type specifiers
        val filteredSpecifiers = fDef.specifiers.filter(specifier => {
            specifier.entry match {
                case spec: OtherSpecifier => true
                case _ => false
            }
        })
        filteredSpecifiers ++ List(Opt(FeatureExprFactory.True, VoidSpecifier()))
    }

    /**
     * Generate the function definition.
     */
    private def genFDef(specs: List[Opt[Specifier]], decl: Declarator, stmts: CompoundStatement,
                        oldStyleParameters: List[Opt[OldParameterDeclaration]] = List()) =
        FunctionDef(specs, decl, oldStyleParameters, stmts)

    /**
     * Generates the opt node for the tunit.
     */
    private def genFDefExternal(oldFDef: FunctionDef, newFDef: FunctionDef, morpheus: Morpheus) =
        Opt[FunctionDef](morpheus.getASTEnv.featureExpr(oldFDef), newFDef)

    /**
     * Generate the function declarator.
     */
    private def genDeclarator(name: String, extensions: List[Opt[DeclaratorExtension]] =
        List[Opt[DeclaratorExtension]]()) = AtomicNamedDeclarator(List[Opt[Pointer]](), Id(name), extensions)
}