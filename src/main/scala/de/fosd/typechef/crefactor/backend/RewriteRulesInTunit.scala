package de.fosd.typechef.crefactor.backend

import de.fosd.typechef.parser.c._
import java.util.Collections
import org.kiama.rewriting.Rewriter._
import scala.Some
import de.fosd.typechef.conditional.Opt
import scala.Some
import de.fosd.typechef.conditional.Opt
import scala.Some
import de.fosd.typechef.conditional.Opt
import scala.Some
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crefactor.Morpheus
import scala.Some
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.parser.c.PointerDerefExpr
import de.fosd.typechef.parser.c.CompoundStatementExpr
import scala.Some
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.parser.c.CompoundStatement
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.NArySubExpr

/**
 * Trait containing all used kiama rewrite rules.
 */
trait RewriteRulesInTunit extends ASTNavigation with ConditionalNavigation {

    /**
     * Replace a list of ids in AST with copied instance with new names.
     */
    def replaceIds[T <: Product](t: T, ids: List[Id], newName: String): T = {
        val idsToReplace = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        ids foreach(idsToReplace.add(_))
        val r = manybu(rule {
            case id: Id => if (idsToReplace.contains(id)) {
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
            case id: Id => if (ids.exists(_ eq id)) PointerDerefExpr(id) else id
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
            case cc: CompoundStatement => if (cc eq cStmt) cc.copy(innerStatements = newInnerStmt) else cc
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    def insertBefore[T <: Product](t: T, mark: Opt[_], insert: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insert :: x :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    /**
     * Inserts one opt statement before and the after a mark in a translation unit.
     */
    def insertBeforeAndAfter[T <: Product](t: T, mark: Opt[_], insertBefore: Opt[_], insertAfter: Opt[_])
                                          (implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insertBefore :: x :: insertAfter :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def insertListBefore[T <: Product](t: T, mark: Opt[_], insert: List[Opt[_]])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) insert ::: x :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replace[T <: Product](t: T, mark: Opt[_], replace: Opt[_])(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) replace :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceStmtInCompoundStatement(ccStmt: CompoundStatement, mark: Opt[Statement], replace: Opt[Statement]) = {
        val newInnerStmts = ccStmt.innerStatements.map { innerStmt =>
            if (innerStmt.eq(mark)) replace
            else innerStmt
        }
        ccStmt.copy(innerStatements = newInnerStmts)
    }

    def replaceOnceTD[T <: Product](t: T, mark: Opt[_], replace: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(mark)) replace :: Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceNArySubExpr[T <: Product](t: T, e: NArySubExpr, n: NArySubExpr)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case i: NArySubExpr => if (isPartOf(i, e)) n else i
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceExprWithCompStmExpr[T <: Product](t: T, e: Expr, n: CompoundStatementExpr)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case i: NArySubExpr => if (isPartOf(i, e)) n else i
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceStmtWithStmtsInCompStmt[T <: Product](t: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]])(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x eq e) Some(n) else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceCompoundStatement[T <: Product](t: T, mark: CompoundStatement, replace: CompoundStatement)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case c: CompoundStatement => if (c eq mark) replace else c
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceId[T <: Product](t: T, e: Id, n: Id)(implicit m: Manifest[T]): T = {
        val r = manybu(rule {
            case i: Id => if (i eq e) n else i
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    def remove[T <: Product](t: T, remove: Opt[_])(implicit m: Manifest[T]): T = {
        val r = oncetd(rule {
            case l: List[Opt[_]] => l.flatMap(x => if (x.eq(remove)) Nil else x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    // Leave them alone - they work!
    def removeStatementInTUnit(l1: List[Opt[Statement]], l2: List[Opt[Statement]]): List[Opt[Statement]] = {
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
            case f: FunctionDef => replaceOnceTD(morpheus.getTranslationUnit, parent,
                parent.copy(entry = f.copy(stmt = workingCallCompStmt)))
            case c: CompoundStatement => replaceCompoundStatement(morpheus.getTranslationUnit, c,
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
