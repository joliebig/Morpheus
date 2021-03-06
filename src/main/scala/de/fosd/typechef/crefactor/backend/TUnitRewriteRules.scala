package de.fosd.typechef.crefactor.backend

import de.fosd.typechef.parser.c._
import java.util.Collections
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.conditional.Opt

/**
 * Trait containing all used kiama rewrite rules that apply the actual transformations.
 * E.g., changing an identifiers.
 */
trait TUnitRewriteRules extends ASTNavigation with ConditionalNavigation {

    /**
     * Replace a list of ids in AST with copied instance with new names.
     */
    def replaceIds[T <: Product](t: T, ids: List[Id], newName: String): T = {
        val idsToReplace = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())
        ids foreach idsToReplace.add
        val r = manybu(rule {
            case id: Id =>
                if (idsToReplace.contains(id)) {
                    // create id with the new name and preserve the position information
                    val copiedId = id.copy(name = newName)
                    id.range match {
                        case Some((from, to)) => copiedId.setPositionRange(from, to)
                        case _ =>
                    }
                    copiedId
                } else
                    id
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    /**
     * Replace a list of ids in AST with copied instances using pointers.
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
                                          newCStmt: CompoundStatement): T = {
        val r = manybu(rule {
            case cc: CompoundStatement => if (cc eq cStmt) newCStmt else cc
            case x => x
        })
        r(t).get.asInstanceOf[T]
    }

    def insertBefore[T <: Product](t: T, mark: Opt[_], insert: Opt[_]): T = {
        val r = oncetd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef].eq(mark))
                        insert :: x :: Nil
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    /**
     * Inserts one opt statement before and the after a mark in a translation unit.
     */
    def insertBeforeAndAfter[T <: Product](t: T, mark: Opt[_], before: Opt[_], after: Opt[_]): T = {
        val r = oncetd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef].eq(mark))
                        before :: x :: after :: Nil
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def insertListBefore[T <: Product](t: T, mark: Opt[_], insert: List[Opt[_]]): T = {
        val r = oncetd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef].eq(mark))
                        insert ::: x :: Nil
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replace[T <: Product](t: T, mark: Opt[_], replace: Opt[_]): T = {
        val r = manybu(rule {
            case l: List[_] => l.flatMap(x =>
                if (x.asInstanceOf[AnyRef].eq(mark))
                    replace :: Nil
                else
                    x :: Nil)
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

    def replaceOnceTD[T <: Product](t: T, mark: Opt[_], replace: Opt[_]): T = {
        val r = oncetd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef].eq(mark))
                        replace :: Nil
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceNArySubExpr[T <: Product](t: T, e: NArySubExpr, n: NArySubExpr): T = {
        val r = manybu(rule {
            case i: NArySubExpr => if (isPartOf(i, e)) n else i
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceExprWithCompStmExpr[T <: Product](t: T, e: Expr, n: CompoundStatementExpr): T = {
        val r = manytd(rule {
            case i: Expr if i eq e => n
        })
        r(t).get.asInstanceOf[T]
    }

    def replaceStmtWithStmtsInCompStmt[T <: Product](t: CompoundStatement, e: Opt[Statement], n: List[Opt[Statement]]): T = {
        val r = manybu(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef] eq e)
                        Some(n)
                    else
                        x :: Nil)
        })
        r(t).get.asInstanceOf[T]
    }

    // generic replace function; possible replacement for replaceId and
    // replaceCompoundStatement, and maybe more?
    def replace[T <: Product, U](t: T, e: U, n: U): T = {
        val r = manybu(rule {
            case i if i.asInstanceOf[AnyRef] eq e.asInstanceOf[AnyRef] => n
        })
        r(t).getOrElse(t).asInstanceOf[T]
    }

    // removes element remove from t, but does not traverse t entirely, since
    // oncetd is top-down traversal, which stops at first successful match
    def remove[T <: Product](t: T, remove: Opt[_]): T = {
        val r = oncetd(rule {
            case l: List[_] =>
                l.flatMap(x =>
                    if (x.asInstanceOf[AnyRef].eq(remove))
                        Nil
                    else
                        x :: Nil)
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

    def insertBefore(l: List[Opt[Statement]], mark: Opt[Statement], insert: Opt[Statement]) =
        l.foldRight(List[Opt[Statement]]())((s, nl) => {
            if (mark.eq(s)) insert :: s :: nl
            else s :: nl
        })

    def replace(morpheus: Morpheus, remove: CompoundStatement,
                            insert: CompoundStatement): TranslationUnit = {
        val parent = parentOpt(remove, morpheus.getASTEnv)
        parent.entry match {
            case f: FunctionDef => replaceOnceTD(morpheus.getTranslationUnit, parent,
                parent.copy(entry = f.copy(stmt = insert)))
            case c: CompoundStatement => replace(morpheus.getTranslationUnit, c,
                c.copy(innerStatements = insert.innerStatements))
                .asInstanceOf[TranslationUnit]
            case _ => throw RefactorException("No valid rewrite rule.")
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
