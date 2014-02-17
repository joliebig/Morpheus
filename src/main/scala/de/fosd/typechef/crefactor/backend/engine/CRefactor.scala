package de.fosd.typechef.crefactor.backend.engine

import de.fosd.typechef.typesystem.CEnvCache
import de.fosd.typechef.crefactor.Morpheus
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.frontend.util.Selection
import de.fosd.typechef.conditional.Conditional
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.parser.c.CompoundStatementExpr
import scala.{Product, Some}
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.conditional.One
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.FunctionDef
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.conditional.Opt

trait CRefactor extends CEnvCache with ASTNavigation with ConditionalNavigation {

    private val VALID_NAME_PATTERN = "[a-zA-Z_][a-zA-Z0-9_]*"

    private val LANGUAGE_KEYWORDS = List("auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local")

    def isAvailable(morpheus: Morpheus, selection: Selection): Boolean

    /**
     * Checks if the name of a variable is compatible to the iso c standard. See 6.4.2 of the iso standard
     *
     * @param name name to check
     * @return <code>true</code> if valid, <code>false</code> if not
     */
    def isValidId(name: String): Boolean =
        name.matches(VALID_NAME_PATTERN) && !name.startsWith("__") && !isReservedLanguageKeyword(name)

    def isLinked(name: String, morpheus: Morpheus): Boolean =
        (morpheus.getLinkInterface != null) && morpheus.getLinkInterface.isListed(name)

    def generateValidNewName(id: Id, stmt: Opt[AST], morph: Morpheus, appendix: Int = 1): String = {
        val newName = id.name + "_" + appendix
        if (isShadowed(newName, stmt.entry, morph)) generateValidNewName(id, stmt, morph, appendix + 1)
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
            case Opt(ft, entry) =>
                featureSet += ft
            case Choice(ft, a, b) =>
                featureSet += ft
        })
        r(a).get
        featureSet.foldRight(FeatureExprFactory.True)((fxpr, setEntry) => fxpr.or(setEntry))
    }

    def buildChoice[T <: AST](attribute: List[(T, FeatureExpr)]): Conditional[T] = {
        if (attribute.isEmpty) One(null.asInstanceOf[T])
        else if (attribute.length == 1) One(attribute.head._1)
        else Choice(attribute.head._2, One(attribute.head._1), buildChoice(attribute.tail))
    }

    def buildVariableCompoundStatement(stmts: List[(CompoundStatementExpr, FeatureExpr)]): CompoundStatementExpr = {
        // move several compoundStatement into one and apply their feature.
        val innerstmts = stmts.foldLeft(List[Opt[Statement]]())((innerstmts, stmtEntry) => stmtEntry._1 match {
            case CompoundStatementExpr(CompoundStatement(inner)) => innerstmts ::: inner.map(stmt => stmt.copy(feature = stmt.feature.and(stmtEntry._2)))
            case _ => innerstmts
        })
        CompoundStatementExpr(CompoundStatement(innerstmts))
    }


    def isShadowed(name: String, element: AST, morpheus: Morpheus): Boolean = {
        val lookupValue = findPriorASTElem[CompoundStatement](element, morpheus.getASTEnv) match {
            case s@Some(x) => x.innerStatements.last.entry
            case _ => morpheus.getTranslationUnit.defs.last.entry
        }

        val env = morpheus.getEnv(lookupValue)

        isDeclaredVarInEnv(name, env.asInstanceOf[Env]) || isDeclaredStructOrUnionInEnv(name, env.asInstanceOf[Env]) || isDeclaredTypeDefInEnv(name, env.asInstanceOf[Env])
    }

    def isDeclaredVarInEnv(name: String, env: Env) = env.varEnv(name) match {
        case One(x) => !x.isUnknown
        case _ => true
    }

    def isDeclaredStructOrUnionInEnv(name: String, env: Env) = env.structEnv.someDefinition(name, false) || env.structEnv.someDefinition(name, true)

    def isDeclaredTypeDefInEnv(name: String, env: Env) = env.typedefEnv(name) match {
        case One(x) => !x.isUnknown
        case _ => true
    }

    /**
     * Replace a list of ids in AST with copied instance with new names.
     */
    def replaceIds[T <: Product](t: T, ids: List[Id], newName: String): T = {
        val r = manybu(rule {
            case id: Id => if (ids.exists(isPartOf(id, _))) id.copy(name = newName) else id
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

    def replaceInASTOnceTD[T <: Product](t: T, mark: Opt[_], replace: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) replace :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    // TODO toRemove; I'm not sure whether the function signature reflects its purpose!
    //                Second and third T should be different!
    def replaceInAST[T <: Product](t: T, e: T, n: T)(implicit m: Manifest[T]): T = {
        println("start replace")
        val r = manybu(rule {
            case i: T => if (isPartOf(i, e)) n else i
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

    def insertBefore(l: List[Opt[Statement]], mark: Opt[Statement], insert: Opt[Statement]) = l.foldLeft(List[Opt[Statement]]())((nl, s) => {
        if (mark.eq(s)) insert :: s :: nl
        else s :: nl
    }).reverse

    def insertRefactoredAST(morpheus: Morpheus, callCompStmt: CompoundStatement, workingCallCompStmt: CompoundStatement): TranslationUnit = {
        val parent = parentOpt(callCompStmt, morpheus.getASTEnv)
        parent.entry match {
            case f: FunctionDef => replaceInASTOnceTD(morpheus.getTranslationUnit, parent, parent.copy(entry = f.copy(stmt = workingCallCompStmt)))
            case c: CompoundStatement => replaceInAST(morpheus.getTranslationUnit, c,
                c.copy(innerStatements = workingCallCompStmt.innerStatements))
                .asInstanceOf[TranslationUnit]
            case x =>
                assert(false, "Something bad happend - i am going to cry, i missed: " + x)
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

