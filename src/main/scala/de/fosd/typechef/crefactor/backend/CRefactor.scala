package de.fosd.typechef.crefactor.backend

import de.fosd.typechef.typesystem.{CType, CEnvCache}
import de.fosd.typechef.crefactor.{Logging, Morpheus}
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.frontend.util.Selection
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.typesystem.linker.SystemLinker

trait CRefactor extends CEnvCache with ASTNavigation with ConditionalNavigation with EnforceTreeHelper with Logging {

    private val VALID_NAME_PATTERN = "[a-zA-Z_][a-zA-Z0-9_]*"

    private val LANGUAGE_KEYWORDS = List("auto", "break", "case", "char", "const", "continue", "default",
        "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long",
        "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch",
        "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic",
        "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local")

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean

    /**
     * Checks if the name of a variable is compatible to the iso c standard. See 6.4.2 of the iso standard
     *
     * @param name name to check
     * @return <code>true</code> if valid, <code>false</code> if not
     */
    def isValidId(name: String): Boolean =
        (name.matches(VALID_NAME_PATTERN)
            && !name.startsWith("__")
            && !isReservedLanguageKeyword(name)
            && !isSystemLinkedName(name))

    def isSystemLinkedName(name: String) = SystemLinker.allLibs.par.contains(name)

    def isValidInProgram(name: Opt[String], morpheus: Morpheus): Boolean =
        (morpheus.getModuleInterface != null) && morpheus.getModuleInterface.isListed(name, morpheus.getFM)

    def generateValidNewName(id: Id, stmt: Opt[AST], morpheus: Morpheus, appendix: Int = 1): String = {
        val newName = id.name + "_" + appendix
        if (isValidInModule(newName, stmt.entry, morpheus)) generateValidNewName(id, stmt, morpheus, appendix + 1)
        else newName
    }

    /**
     * Checks if the name is a language keyword.
     *
     * @param name the name to check
     * @return <code>true</code> if language keyword
     */
    def isReservedLanguageKeyword(name: String) = LANGUAGE_KEYWORDS.contains(name)

    def getOrFeatures(a: Any): FeatureExpr = {
        var featureSet: Set[FeatureExpr] = Set()
        val r = manytd(query {
            case Opt(ft, _) => featureSet += ft
            case Choice(ft, _, _) => featureSet += ft
        })
        r(a).get
        featureSet.foldRight(FeatureExprFactory.True)((fxpr, setEntry) => fxpr.or(setEntry))
    }


    // TODO maybe replace with conditionalFoldRight see ConditionalLib
    def buildChoice[T <: AST](attribute: List[(T, FeatureExpr)]): Conditional[T] = {
        if (attribute.isEmpty) One(null.asInstanceOf[T])
        else if (attribute.length == 1) One(attribute.head._1)
        else Choice(attribute.head._2, One(attribute.head._1), buildChoice(attribute.tail))
    }

    def buildVariableCompoundStatement(stmts: List[(CompoundStatementExpr, FeatureExpr)]): CompoundStatementExpr = {
        // move several compoundStatement into one and apply their feature.
        val innerstmts = stmts.foldLeft(List[Opt[Statement]]())((innerstmts, stmtEntry) => stmtEntry._1 match {
            case CompoundStatementExpr(CompoundStatement(inner)) =>
                innerstmts ::: inner.map(stmt => stmt.copy(feature = stmt.feature.and(stmtEntry._2)))
            case _ => innerstmts
        })
        CompoundStatementExpr(CompoundStatement(innerstmts))
    }


    def isValidInModule(name: String, element: AST, morpheus: Morpheus): Boolean = {
        val lookupValue = findPriorASTElem[CompoundStatement](element, morpheus.getASTEnv) match {
            case Some(x) => x.innerStatements.last.entry
            case _ => morpheus.getTranslationUnit.defs.last.entry
        }

        val env = morpheus.getEnv(lookupValue).asInstanceOf[Env]
        val ctx = morpheus.getASTEnv.featureExpr(element)

        (isDeclaredInEnv(env.varEnv(name), ctx, morpheus)
            || isDeclaredStructOrUnionInEnv(name, env)
            || isDeclaredInEnv(env.typedefEnv(name), ctx, morpheus))
    }

    private def isDeclaredInEnv(env: Conditional[CType], ctx: FeatureExpr, morpheus: Morpheus):
    Boolean = {
        !ConditionalLib.items(env).forall {
            x => x._2.isUnknown || (ctx and x._1 isContradiction morpheus.getFM)
        }
    }

    private def isDeclaredStructOrUnionInEnv(name: String, env: Env): Boolean = {
        env.structEnv.someDefinition(name, false) || env.structEnv.someDefinition(name, true)
    }

    /**
     * Replace a list of ids in AST with copied instance with new names.
     */
    def replaceIds[T <: Product](t: T, ids: List[Id], newName: String): T = {
        val r = manybu(rule {
            case id: Id => if (ids.exists(isPartOf(id, _))) {
                val copiedId = id.copy(name = newName)
                val orgPosRange = id.range
                orgPosRange match {
                    case Some(range) => copiedId.setPositionRange(range._1, range._2)
                    case _ =>
                }
                copiedId
            } else id
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    /**
     * Replace a list of ids in AST with copied instance with as pointer.
     */
    def replaceIdsWithPointers[T <: Product](t: T, ids: List[Id]): T = {
        val r = manybu(rule {
            case id: Id => if (ids.exists(isPartOf(id, _))) PointerDerefExpr(id) else id
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    /**
     * Replaces the innerstatements of compoundstatements of a translation unit.
     */
    def replaceCompoundStmt[T <: Product](t: T, cStmt: CompoundStatement,
                                          newInnerStmt: List[Opt[Statement]]): T = {
        val r = manybu(rule {
            case cc: CompoundStatement => if (isPartOf(cStmt, cc)) cc.copy(innerStatements = newInnerStmt) else cc
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    // TODO Clean up tunit rewrite strategies
    def insertInAstBefore[T <: Product](t: T, mark: Opt[_], insert: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insert :: x :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def insertInAstBeforeBU[T <: Product](t: T, mark: Opt[_], insert: List[Opt[_]])(implicit m: Manifest[T]): T = {
        val r = all(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insert ::: x :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def insertInAstBeforeBU[T <: Product](t: T, mark: Opt[_], insert: Opt[_])(implicit m: Manifest[T]): T = {
        val r = alltd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insert :: x :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def insertInAstBefore[T <: Product](t: T, mark: Opt[_], insert: List[Opt[_]])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insert ::: x :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceInAST[T <: Product](t: T, mark: Opt[_], replace: Opt[_])(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) replace :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceStmtInCompoundStatement(ccStmt: CompoundStatement,mark: Opt[Statement], replace: Opt[Statement]) = {
        val newInnerStmts = ccStmt.innerStatements.map { innerStmt =>
            if (innerStmt.eq(mark)) replace
            else innerStmt
        }
        ccStmt.copy(innerStatements = newInnerStmts)
    }

    def replaceInASTOnceTD[T <: Product](t: T, mark: Opt[_], replace: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) replace :: Nil else x :: Nil)
        })
        println(mark)
        println(replace)
        r(t).get.asInstanceOf[T]
    }

    // TODO toRemove; I'm not sure whether the function signature reflects its purpose!
    //                Second and third T should be different!
    def replaceInAST[T <: Product](t: T, e: T, n: T)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case i: T => if (isPartOf(i, e)) n else i
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceCompoundStatementInTUnit[T <: Product](t: T, mark: CompoundStatement, replace: CompoundStatement)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case c: CompoundStatement => if (c eq mark) replace else c
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceInAST[T <: Product](t: T, e: Id, n: Expr)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case i: Id => if (isPartOf(i, e)) n else i
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    def removeFromAST[T <: Product](t: T, remove: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(remove)) Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def removeFromASTbu[T <: Product](t: T, remove: Opt[_])(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(remove)) Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    // Leave them alone - they work!
    def eqRemove(l1: List[Opt[Statement]], l2: List[Opt[Statement]]): List[Opt[Statement]] = {
        l1.flatMap(x => l2.exists(y => x.eq(y)) match {
            case true => None
            case false => Some(x)
        })
    }

    // -TODO: @andreas use foldRight without the reverse??
    def insertBefore(l: List[Opt[Statement]], mark: Opt[Statement], insert: Opt[Statement]) =
        l.foldLeft(List[Opt[Statement]]())((nl, s) => {
            if (mark.eq(s)) insert :: s :: nl
            else s :: nl
        }).reverse

    def insertRefactoredAST(morpheus: Morpheus, callCompStmt: CompoundStatement,
                            workingCallCompStmt: CompoundStatement): TranslationUnit = {
        val parent = parentOpt(callCompStmt, morpheus.getASTEnv)
        parent.entry match {
            case f: FunctionDef => replaceInASTOnceTD(morpheus.getTranslationUnit, parent,
                parent.copy(entry = f.copy(stmt = workingCallCompStmt)))
            case c: CompoundStatement => replaceCompoundStatementInTUnit(morpheus.getTranslationUnit, c,
                c.copy(innerStatements = workingCallCompStmt.innerStatements))
                .asInstanceOf[TranslationUnit]
            case x =>
                assert(false, "Something bad happened; missing: " + x)
                morpheus.getTranslationUnit
        }
    }

    private def isPartOf(subterm: Product, term: Any): Boolean = {
        term match {
            case _: Product if subterm.asInstanceOf[AnyRef].eq(term.asInstanceOf[AnyRef]) => true
            case l: List[_] => l.map(isPartOf(subterm, _)).exists(_ == true)
            case p: Product => p.productIterator.toList.map(isPartOf(subterm, _)).exists(_ == true)
            case x => false
        }
    }
}

case class RefactorException(error: String) extends Exception

