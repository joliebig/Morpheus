package de.fosd.typechef.crefactor.backend.engine

import java.util.Collections

import org.kiama.rewriting.Rewriter._

import de.fosd.typechef.conditional._
import de.fosd.typechef.crewrite.IntraCFG
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._

import de.fosd.typechef.crefactor.backend.{RefactorException, CRefactor}
import de.fosd.typechef.crefactor.evaluation_utils.Configuration
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.evaluation.StatsCan

/**
 * Implements extract-function refactoring.
 */
object CExtractFunction extends CRefactor with IntraCFG {

    // TODO Integrate in extract
    def canRefactor(morpheus: Morpheus, selection: List[AST]): Boolean = {
        if (selection.isEmpty) return false

        val selectedIds = filterAllASTElems[Id](selection)
        val externalUses = morpheus.getExternalUses(selectedIds)
        val externalDefs = morpheus.getExternalDefs(selectedIds)
        val liveIds = uniqueExtRefIds(externalDefs, externalUses)

        val extractSelection = CExtractSelection(selectedIds, externalUses, externalDefs, liveIds)

        if (!selection.par.forall { findPriorASTElem[FunctionDef](_, morpheus.getASTEnv).isDefined }) false
        else if (!isPartOfSameCompStmt(selection, morpheus)) false
        else if (!filterAllASTElems[ReturnStatement](selection).isEmpty) false
        else if (!selection.par.forall(checkAstElemForCFGDisruption(_, selection, morpheus))) false
        else if (hasIdsWithDifferentScope(extractSelection, morpheus)) false
        else if (hasInvisibleEnumerations(extractSelection, morpheus)) false
        else if (hasInvisibleStructOrTypeDefSpecifier(extractSelection, morpheus)) false
        else if (hasVariablesDeclaredWithRegisterSpecifier(extractSelection, morpheus)) false
        else true
    }

    def extract(morpheus: Morpheus, selectedElements: List[AST], funcName: String):
    Either[String, (TranslationUnit, Opt[FunctionDef])] = {

        if (!isValidName(funcName))
            return Left(Configuration.getInstance().getConfig("default.error.invalidName"))

        // Reference check is performed as soon as we know the featureExpr the new function is going to have!

        val oldFDef = findPriorASTElem[FunctionDef](selectedElements.head, morpheus.getASTEnv)
        if (!isValidInProgram(Opt(morpheus.getASTEnv.featureExpr(oldFDef.get), funcName), morpheus))
            return Left(Configuration.getInstance().getConfig(
                "default.error.isInConflictWithSymbolInModuleInterface"))

        // we check binding and visibility using the last element in the translation unit
        if (!isValidInModule(funcName, morpheus.getTranslationUnit.defs.last.entry, morpheus))
            return Left(Configuration.getInstance().getConfig("default.error.isInConflictWithSymbolInModule"))

        // we can only handle statements. report error otherwise.
        if (selectedElements.exists {
            case _: Expr => true
            case _ => false
        })
            return Left(Configuration.getInstance().getConfig("refactor.extractFunction.failed.unsupported"))

        if (!selectedElements.forall {
            case _: Statement => true
            case _ => false
        })
            return Left(Configuration.getInstance().getConfig("refactor.extractFunction.failed.invalidSelection"))

        extractStatements(morpheus, selectedElements, funcName)
    }

    private def extractStatements(morpheus: Morpheus, selection: List[AST], fName: String):
    Either[String, (TranslationUnit, Opt[FunctionDef])] = {
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

            val externalUses = morpheus.getExternalUses(selectedIds)
            val externalDefs = morpheus.getExternalDefs(selectedIds)
            val allExtRefIds = externalDefs.flatMap(x => Some(x._1))
            val extRefIds = uniqueExtRefIds(externalDefs, externalUses)
            val toDeclare = getIdsToDeclare(externalUses)

            // usages of declared variables outside the selection is not supported yet
            // in case we reorder the elements it may be possible to apply the refactoring.
            if (!toDeclare.isEmpty)
                return Left("Invalid selection, a declared variable in the selection gets used outside.")

            val params = retrieveParameters(removeVisibleLiveParameters(extRefIds, morpheus), morpheus)
            val paramsIds = params.map(_._3)

            StatsCan.addStat(morpheus.getFile, Liveness,      startTime.getTime)
            StatsCan.addStat(morpheus.getFile, ExternalUses,  externalUses)
            StatsCan.addStat(morpheus.getFile, ExternalDecls, externalDefs)
            StatsCan.addStat(morpheus.getFile, Parameters,    paramsIds)

            // generate new function definition
            val specifiers = genSpecifiers(parentFunction, morpheus)
            val parameterDecls = getParameterDecls(params, parentFunction, morpheus)
            val declarator = genDeclarator(fName, parameterDecls)
            val compoundStatement = genCompoundStatement(selectedOptStatements,
                allExtRefIds, paramsIds, morpheus)
            val newFDef = genFDef(specifiers, declarator, compoundStatement)
            val newFDefOpt = genFDefExternal(parentFunction, newFDef, morpheus)

            // generate forward declaration
            val newFDefForward = genFDefForward(parentFunction, morpheus, specifiers, declarator)
            val newFDefForwardOpt = genFDefExternal(parentFunction, newFDefForward, morpheus)

            if (!isValidInProgram(Opt(newFDefOpt.feature, fName), morpheus))
                return Left(Configuration.getInstance().getConfig("default.error.invalidName"))

            // generate function fCall
            val callParameters = genFCallParams(params)
            val functionCall = Opt[ExprStatement](newFDefOpt.feature,
                ExprStatement(PostfixExpr(Id(newFDefOpt.entry.getName),
                    FunctionCall(ExprList(callParameters)))))

            // Keep changes at the AST as local as possible
            // TODO: Check with single line compound statements.
            val tunitWithFCall = insertBefore(compStmt.innerStatements,
                selectedOptStatements.head, functionCall)
            val ccStmtWithRemovedStmts = removeStatementInTUnit(tunitWithFCall, selectedOptStatements)
            val tunitWithFDef = insertBeforeAndAfter(morpheus.getTranslationUnit,
                parentFunctionOpt, newFDefForwardOpt, newFDefOpt)

            val refAST = replaceCompoundStmt(tunitWithFDef, compStmt, compStmt.copy(innerStatements = ccStmtWithRemovedStmts))
            Right(refAST, newFDefOpt)
        } catch {
            case r: RefactorException => Left(r.error)
            case x: Throwable =>
                x.printStackTrace()
                Left(x.getMessage)
        }
    }

    private def hasVariablesDeclaredWithRegisterSpecifier(selection: CExtractSelection, morpheus: Morpheus): Boolean = {
        def containsRegisterSpecifier(specs : List[Opt[Specifier]]) : Boolean = {
             specs.exists(_.entry match {
                 case r: RegisterSpecifier => true
                 case _ => false
             })
        }

        selection.liveIds.exists(id => morpheus.getDecls(id).exists(decl => {
            val declaration = findPriorASTElem[Declaration](decl, morpheus.getASTEnv)
            val registeredInDecl = {
                declaration match {
                    case Some(entry) => containsRegisterSpecifier(entry.declSpecs)
                    case _ => false
                }
            }

            val paramDecl = findPriorASTElem[ParameterDeclaration](decl, morpheus.getASTEnv)
            val registerInParamDecl = {
                paramDecl match {
                    case Some(entry) => containsRegisterSpecifier(entry.specifiers)
                    case _ => false
                }
            }

            registerInParamDecl || registeredInDecl

        }))
    }

    private def hasInvisibleStructOrTypeDefSpecifier(selection: CExtractSelection, morpheus: Morpheus): Boolean =
        selection.selectedIds.exists(id => {
            morpheus.getDecls(id).::(id).exists({
                declId => {
                    // We can only look at look declaration as parameter typedef are forced to be visible to the
                    // extracted function.
                    val decl = findPriorASTElem[Declaration](declId, morpheus.getASTEnv)
                    decl match {
                        case None => false
                        case Some(entry) => entry.declSpecs.exists(spec => {
                            spec.entry match {
                                case TypeDefTypeSpecifier(i: Id) =>
                                    val invisible = morpheus.getDecls(i).exists(findPriorASTElem[CompoundStatement](_, morpheus.getASTEnv) match {
                                        case None => false
                                        case _ => true
                                    })
                                    val funcCall = morpheus.getReferences(declId).exists(ref => morpheus.isPartOfFuncCall(ref.entry))
                                    invisible || funcCall
                                case s: StructOrUnionSpecifier => {
                                    val idIsInvisible = s.id match {
                                        case None => true
                                        case Some(id) => morpheus.getDecls(id).exists(findPriorASTElem[CompoundStatement](_, morpheus.getASTEnv) match {
                                            case None => false
                                            case _ => true
                                        })
                                    }
                                    val structDeclIsInvisible = s.enumerators match {
                                        case None => false
                                        case Some(structDeclarations) => structDeclarations.exists(structDecl =>
                                            findPriorASTElem[CompoundStatement](structDecl.entry, morpheus.getASTEnv) match {
                                                case None => false
                                                case _ => true
                                            })
                                    }
                                    idIsInvisible || structDeclIsInvisible
                                }
                                case _ => false
                            }
                        })
                    }
                }
            })
        })

    private def hasInvisibleEnumerations(selection: CExtractSelection, morpheus: Morpheus): Boolean = {
        val invisibleEnums = selection.liveIds.exists(liveId => {
            try {
                val scope = findPriorASTElem[CompoundStatement](liveId, morpheus.getASTEnv) match {
                    case Some(x) => x.innerStatements.last.entry
                    case _ => morpheus.getTranslationUnit.defs.last.entry
                }
                val enums = ConditionalLib.leaves(morpheus.getEnv(scope).varEnv.lookup(liveId.name))
                val res = enums.exists {case (_, KEnumVar, 1, _) => true; case _ => false}

                if (res)
                    logger.info(liveId + " is invisible after extraction")

                res
            } catch {
                case _: Throwable =>
                    // logger.warn("No entry found for: " + liveId)
                    false
            }
        })

        if (invisibleEnums)
            logger.info("Not available for extract - has invisible enums.")

        invisibleEnums
    }

    private def hasIdsWithDifferentScope(selection: CExtractSelection, morpheus: Morpheus): Boolean = {
        val externalUses = morpheus.getExternalUses(selection.selectedIds)
        val idsToDeclare = getIdsToDeclare(externalUses)

        !idsToDeclare.isEmpty
    }

    private def getParameterDecls(parameters: List[(Opt[ParameterDeclaration], Opt[Expr], Id)],
                                  fDef: FunctionDef, morpheus: Morpheus) = {
        val decls = parameters.map(_._1)
        List[Opt[DeclaratorExtension]](Opt(parentOpt(fDef, morpheus.getASTEnv).feature,
            DeclParameterDeclList(decls)))
    }

    private def retrieveParameters(liveParamIds: List[Id], morpheus: Morpheus):
    List[(Opt[ParameterDeclaration], Opt[Expr], Id)] = {
        val declIdMap: java.util.IdentityHashMap[Declaration, List[Id]] =
            new java.util.IdentityHashMap
        val declFeatureMap: java.util.IdentityHashMap[Declaration, FeatureExpr] =
            new java.util.IdentityHashMap
        val declDeclPointerMap: java.util.IdentityHashMap[Declaration, List[Declarator]] =
            new java.util.IdentityHashMap
        val idFeatureMap: java.util.HashMap[String, List[FeatureExpr]] =
            new java.util.HashMap


        def addToIdFeatureMap(id: Id, feature: FeatureExpr) = {
            if (!idFeatureMap.containsKey(id.name))
                idFeatureMap.put(id.name, List(feature))
            else
                idFeatureMap.put(id.name, feature :: idFeatureMap.get(id.name))
        }

        def addTodeclIdMapMap(decl: Declaration, id: Id) = {
            if (declIdMap.containsKey(decl)) declIdMap.put(decl, id :: declIdMap.get(decl))
            else declIdMap.put(decl, List(id))
        }

        def addToDeclFeatureMap(decl: Declaration, declFeature: FeatureExpr) =
            if (declFeatureMap.containsKey(decl))
                declFeatureMap.put(decl, declFeature.and(declFeatureMap.get(decl)))
            else declFeatureMap.put(decl, declFeature)

        def addTodeclDeclPointerMap(decl: Declaration, declarator: Declarator) = {
            if (!declDeclPointerMap.containsKey(decl))
                declDeclPointerMap.put(decl, List(declarator))
            else declDeclPointerMap.put(decl, declarator :: declDeclPointerMap.get(decl))
        }

        def isAlreadyKnownAsParameter(id: Id, feature: FeatureExpr): Boolean = {
            if (idFeatureMap.containsKey(id.name))
                idFeatureMap.get(id.name).exists(fxpr =>
                    feature.equivalentTo(fxpr, morpheus.getFM))
            else
                false
        }

        /**
         * Adds usual decls to possible parameters
         */
        def addDeclToDeclsToGenerate(feature: FeatureExpr, decl: Declaration, id: Id): Any = {
            if (!isAlreadyKnownAsParameter(id, feature) && !containsEnumsSpecifier(decl)) {
                addToIdFeatureMap(id, feature)
                addToDeclFeatureMap(decl, feature)
                addTodeclDeclPointerMap(decl, generateInit(decl, id))
                addTodeclIdMapMap(decl, id)
            }
        }

        def addParameterFromParameter(id: Id, ft: FeatureExpr, fallbackAllowed: Boolean = true, featureExploit: Boolean = true) = {

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
                case Some(p: ParameterDeclarationD) =>
                    val feature = {
                        if (featureExploit) retrieveAllDeclParameterFeatures(p, ft)
                        else ft
                    }
                    val genDeclFromPDecl = Declaration(p.specifiers, List(Opt(ft, InitDeclaratorI(p.decl, p.attr, None))))
                    addDeclToDeclsToGenerate(feature, genDeclFromPDecl, id)
                case x =>
                    // Fallback to other determination method -> no clear informations from type env.
                    if (fallbackAllowed) addParameterFromDeclaration(id, ft, !fallbackAllowed, featureExploit)
                    else logger.warn("Would have requiered fallback to declaration: " + id)
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
                else decl.declSpecs.foldLeft((List[Opt[Pointer]](), List[FeatureExpr]())) {(entries, declSpec) => genPointer(entries, declSpec)}._1

            val resPointers = decl.init.foldLeft(genPointers) {(currentPointers, declInit) => {
                if(declInit.entry.getName.eq(param.name)) declInit.entry.declarator.pointers ::: currentPointers
                else currentPointers
            }}

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

        /**
         * Helping method for fake choice nodes retrieved from typesystem.
         * Fake choice nodes are nodes representing variability of the same exact element under different features.
         * Consider the following example:
         * int foo(unit_8 a, int b) with unit_8 as variable TypeDef defined in a header file under
         * two different features A and B.
         * Due to code code duplication of TypeChef int b would be also variable with following choices: A, !A, B, !B
         * This has caused some compile issues and therefore we put all choices to a big OR feature.
         */
        def addChoiceOne(c: Choice[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True) = {
            def getOrFeatures(a: Any): FeatureExpr = {
                var featureSet: Set[FeatureExpr] = Set()
                val r = manytd(query {
                    case Opt(ft, _) => featureSet += ft
                    case Choice(ft, _, _) => featureSet += ft
                })
                r(a).get
                featureSet.foldRight(FeatureExprFactory.True)(_ or _)
            }

            val orFeatures = getOrFeatures(c)
            val featureExpr = ft.and(orFeatures)
            addParameterFromDeclaration(id, featureExpr, true, false)
        }

        def containsEnumsSpecifier(entry: Declaration): Boolean = {
            entry.declSpecs.exists(_.entry match {
                case _: EnumSpecifier => true
                case _ => false
            })
        }

        def addParameterFromDeclaration(id: Id, ft: FeatureExpr, fallBackAllowed: Boolean = true, featureExploit: Boolean = true) {
            val decl = findPriorASTElem[Declaration](id, morpheus.getASTEnv)
            decl match {
                case Some(entry) => {
                    val feature = if (ft.equivalentTo(FeatureExprFactory.True))
                        parentOpt(entry, morpheus.getASTEnv).feature
                    else
                        ft
                    // TODO Possible Scala Bug?
                    // addDeclToDeclsToGenerate(feature, entry, id)
                    if (!isAlreadyKnownAsParameter(id, feature) && !containsEnumsSpecifier(entry)) {
                        addToIdFeatureMap(id, feature)
                        addToDeclFeatureMap(entry, feature)
                        addTodeclDeclPointerMap(entry, generateInit(entry, id))
                        addTodeclIdMapMap(entry, id)
                    }
                }
                case None =>
                    // fallback as parameter from parameter...
                    if (fallBackAllowed) addParameterFromParameter(id, ft, !fallBackAllowed, featureExploit)
                    else logger.warn("Would have required fallback to parameter: " + id)
            }
        }

        // direct enum use -> check for visibility only as enums are constant
        // if not visible afterwards the refactoring can not be made.
        def isInvisibleEnum(id: Id) =  {
            morpheus.getDecls(id).exists(findPriorASTElem[CompoundStatement](_, morpheus.getASTEnv) match {
                case None => false
                case _ => true
            })
        }

        def addOne(o: One[_], id: Id, ft: FeatureExpr = FeatureExprFactory.True) = {
            if (ft.isTautology(morpheus.getFM)) {
                o match {
                    // only variables are interesting
                    case One((CUnknown(_), _, _)) =>
                    case One((CFunction(_, _), _, _)) =>
                    case One((CType(CFunction(_, _), _, _, _), _, _, ExternalLinkage)) =>
                    case One((CType(CFunction(_, _), _, _, _), _, _, InternalLinkage)) =>
                    case One((_, KEnumVar, _, _)) => if (isInvisibleEnum(id)) {
                        throw new RefactorException("Type Declaration for " + id.name +
                            " would be invisible after extraction!")
                    }
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
                val scope = findPriorASTElem[CompoundStatement](liveId, morpheus.getASTEnv) match {
                    case Some(x) => x.innerStatements.last.entry
                    case _ => morpheus.getTranslationUnit.defs.last.entry
                }
                morpheus.getEnv(scope).varEnv.lookup(liveId.name) match {
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
                case x: Throwable => logger.warn("No entry found for: " + liveId)
            })

        val decls = declFeatureMap.keySet().toArray(Array[Declaration]()).toList
        decls.flatMap(decl => {
            val feature = decls.foldLeft(declFeatureMap.get(decl))(f = (ft, otherDecl) => {
                if (declDeclPointerMap.get(decl).exists(
                    declarator => declDeclPointerMap.get(otherDecl).exists(_.getName.equals(declarator.getName)))
                    && !decl.eq(otherDecl)) {
                    val andFeature = declFeatureMap.get(otherDecl).not()
                    if (!andFeature.equivalentTo(FeatureExprFactory.False)) ft.and(andFeature)
                    else ft
                } else ft
            })

            decl.declSpecs.foreach(spec => {
                spec.entry match {
                    case TypeDefTypeSpecifier(i: Id) =>
                        if (morpheus.getDecls(i).exists(findPriorASTElem[CompoundStatement](_, morpheus.getASTEnv) match {
                            case None => false
                            case _ => true
                        })) throw new RefactorException("Type Declaration for " + i +
                            " would be invisible after extraction!")
                    case s: StructOrUnionSpecifier => {
                        s.id match {
                            case None => throw new RefactorException("Anonymous struct declaration are not supported")
                            case Some(id) => if (morpheus.getDecls(id).exists(findPriorASTElem[CompoundStatement](_, morpheus.getASTEnv) match {
                                case None => false
                                case _ => true
                            })) throw new RefactorException("Struct Declaration for " + id +
                                " would be invisible after extraction!")
                        }
                        s.enumerators match {
                            case None =>
                            case Some(structDeclarations) => if (structDeclarations.exists(structDecl => {
                                findPriorASTElem[CompoundStatement](structDecl.entry, morpheus.getASTEnv) match {
                                    case None => false
                                    case _ => true
                                }
                            })) throw new RefactorException("Struct Declarations for " + structDeclarations +
                                " would be invisible after extraction!")
                        }
                    }
                    case _ => logger.debug("Specs " + spec)
                }
            })

            // remove extern specifier and struct declartors from function argument.
            val filteredDeclSpecs = decl.declSpecs.flatMap(current => current.entry match {
                case c: ConstSpecifier => Some(current)
                case sd: StructOrUnionSpecifier => Some(current.copy(entry = sd.copy(enumerators = None)))
                case s: OtherSpecifier => None
                case _ => Some(current)
            })


            val ids = declIdMap.get(decl)
            ids.flatMap {
                id =>
                    def getDeclarator(declarators: List[Declarator]): Declarator = {
                        declarators match {
                            case Nil => null
                            case head :: tail =>
                                if (head.getName.equals(id.name)) head
                                else getDeclarator(tail)
                        }
                    }
                    val declarator = getDeclarator(declDeclPointerMap.get(decl))
                    val paramDecl = Opt(feature, ParameterDeclarationD(filteredDeclSpecs,
                        declarator, List()))
                    val expr =  {
                        if (isArray(id, morpheus)) Opt(feature, Id(id.name))
                        else Opt(feature, PointerCreationExpr(Id(id.name)))
                    }
                    Some((paramDecl, expr, id))
            }
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
        parameterIds.toArray(Array[Id]()).toList.sortWith {(i1, i2) => i1.name < i2.name}
    }

    private def getIdsToDeclare(uses: List[(Id, List[Id])]) = {
        val declarationIds = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        uses.foreach(id => declarationIds.add(id._1))
        declarationIds.toArray(Array[Id]()).toList.sortWith {(i1, i2) => i1.name < i2.name}
    }

    /**
     * Removes all visible parameters from the liveness list.
     * A visible parameter is a variable determined to be declared outside of the selected statements,
     * but gets declared inside a struct or union. These declarations are visible inside the extracted statements
     * as we need to pass the parent struct or union as parameter to preserve type safety.
     *
     * eg:
     *
     * struct person {
     *     int age;
     * }
     *
     * By extracting the following lines
     *  p->age = 10;
     *
     * we would detect p and age as live variables. But only p can be introduced as parameter of the extracted function,
     * as age is already visible by this parameter.
     */
    private def removeVisibleLiveParameters(liveParamIds: List[Id], morpheus: Morpheus): List[Id] =
        liveParamIds.filter(id => {
            findPriorASTElem[StructOrUnionSpecifier](id, morpheus.getASTEnv) match {
                case Some(s: StructOrUnionSpecifier) => !s.enumerators.exists(enum => isPartOf(id, enum))
                case None => true
            }
        })

    private def genCompoundStatement(statements: List[Opt[Statement]], externalRef: List[Id],
                                     parameters: List[Id], morpheus: Morpheus): CompoundStatement = {
        val variables = externalRef.par.filter(isPartOfParameter(_, parameters, morpheus)).filter(!isArray(_, morpheus)).toList
        // transform input ids into pointer expressions
        val idsAsPointer = replaceIdsWithPointers(statements, variables)
        CompoundStatement(idsAsPointer)
    }

    private def isPartOfParameter(id: Id, parameters: List[Id], morpheus: Morpheus): Boolean = {
        if (!morpheus.isInUseDeclMap(id)) false
        morpheus.getDecls(id).exists(decl => parameters.exists(param => param.eq(decl)))
    }

    private def isArray(id: Id, morpheus: Morpheus) : Boolean = {
        if (!morpheus.isInUseDeclMap(id)) {
            findPriorASTElem[Declarator](id, morpheus.getASTEnv) match {
                case Some(entry) => entry.extensions.exists { extension =>
                    extension.entry match {
                        case d: DeclArrayAccess => true
                        case _ => false
                    }
                }
                case _ => false
            }
        }
        else morpheus.getDecls(id).exists(isArray(_, morpheus))
    }

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

        if (!filteredCStmtsSuccs.forall(isPartOf(_, selection)))
            return false

        // determine all break statements and check whether their jump targers
        // are part of the selection
        val bStmts = filterAllASTElems[BreakStatement](element)
        val succsBStmts = bStmts.flatMap(succ(_, morpheus.getASTEnv))
        val filteredBStmtsSuccs = succsBStmts.filter(_.feature isSatisfiable morpheus.getFM)

        if (!filteredBStmtsSuccs.par.forall(isPartOf(_, selection)))
            return false

        // determine all case statements and check whether their predecessor elements, in particular,
        // the entire switch statement is part of the selection
        val caStmts = filterAllASTElems[CaseStatement](element)
        val predsCaStmts = caStmts.flatMap(pred(_, morpheus.getASTEnv))
        val filteredCaStmts = predsCaStmts.filter(_.feature isSatisfiable morpheus.getFM)

        if (!filteredCaStmts.par.forall(isPartOf(_, selection)))
            return false

        // determine goto and label statements and check whether their references (succs resp. preds)
        // belong to the selection
        val gotoStmts = filterAllASTElems[GotoStatement](element)
        val succsGotoStmts = gotoStmts.flatMap(succ(_, morpheus.getASTEnv))
        val filteredGotoStmts = succsGotoStmts.filter(_.feature isSatisfiable morpheus.getFM)

        if (!filteredGotoStmts.par.forall(isPartOf(_, selection)))
            return false

        val labelStmts = filterAllASTElems[LabelStatement](element)
        val predsLabelStmts = labelStmts.flatMap(pred(_, morpheus.getASTEnv))
        val filteredLabelStmts = predsLabelStmts.filter(_.feature isSatisfiable morpheus.getFM)

        if (!filteredLabelStmts.par.forall(isPartOf(_, selection)))
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
     * Generate the forward declaration
     */
    private def genFDefForward(oldFDef: FunctionDef, morpheus : Morpheus,
                               specs: List[Opt[Specifier]], decl: Declarator,
                               attributes: List[Opt[AttributeSpecifier]] = List(), i: Option[Initializer] = None) =
        Declaration(specs, List(Opt(morpheus.getASTEnv.featureExpr(oldFDef), InitDeclaratorI(decl, attributes, i))))


    /**
     * Generates the opt node for the tunit.
     */
    private def genFDefExternal[T <: AST](oldFDef: FunctionDef, newFDef: T, morpheus: Morpheus) =
        Opt[T](morpheus.getASTEnv.featureExpr(oldFDef), newFDef)

    /**
     * Generate the function declarator.
     */
    private def genDeclarator(name: String, extensions: List[Opt[DeclaratorExtension]] =
        List[Opt[DeclaratorExtension]]()) = AtomicNamedDeclarator(List[Opt[Pointer]](), Id(name), extensions)
}

case class CExtractSelection(selectedIds : List[Id], externalUses: List[(Id, List[Id])],
                             externalDefs: List[(Id, List[Id])], liveIds : List[Id]) {}