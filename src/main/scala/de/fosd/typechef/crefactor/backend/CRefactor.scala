package de.fosd.typechef.crefactor.backend

import de.fosd.typechef.typesystem.{CType, CEnvCache}
import de.fosd.typechef.crefactor.{Logging, Morpheus}
import org.kiama.rewriting.Rewriter._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import de.fosd.typechef.typesystem.linker.SystemLinker
import java.util.Collections

trait CRefactor
    extends CEnvCache with ASTNavigation with ConditionalNavigation with TUnitRewriteRules
    with EnforceTreeHelper with Logging {

    private val REGEX_VALID_IDENTIFIER = "[a-zA-Z_][a-zA-Z0-9_]*"

    private val LANGUAGE_KEYWORDS = List("auto", "break", "case", "char", "const", "continue", "default",
        "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long",
        "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch",
        "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic",
        "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local")

    def isAvailable(morpheus: Morpheus, selection: CodeSelection): Boolean

    /**
     * Checks if the name of a variable is compatible to the iso c standard. See 6.4.2 of the iso standard
     *
     * @param name name to check
     * @return <code>true</code> if valid, <code>false</code> if not
     */
    def isValidId(name: String): Boolean =
        (name.matches(REGEX_VALID_IDENTIFIER)
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
}

case class RefactorException(error: String) extends Exception

