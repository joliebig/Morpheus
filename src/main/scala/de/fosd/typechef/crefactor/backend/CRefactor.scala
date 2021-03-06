package de.fosd.typechef.crefactor.backend

import de.fosd.typechef.typesystem.{CType, CEnvCache}
import de.fosd.typechef.crefactor.{Logging, Morpheus}
import de.fosd.typechef.parser.c._
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.typesystem.linker.SystemLinker
import java.io.File
import de.fosd.typechef.crefactor.evaluation.Evaluation

trait CRefactor
    extends CEnvCache with ASTNavigation with ConditionalNavigation with TUnitRewriteRules
    with EnforceTreeHelper with Logging {

    private val REGEX_VALID_IDENTIFIER = "[a-zA-Z_][a-zA-Z0-9_]*"

    private val LANGUAGE_KEYWORDS = List("auto", "break", "case", "char", "const", "continue", "default",
        "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long",
        "register", "restrict", "return", "short", "signed", "sizeof", "static", "struct", "switch",
        "typedef", "union", "unsigned", "void", "volatile", "while", "_Alignas", "_Alignof", "_Atomic",
        "_Bool", "_Complex", "_Generic", "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local")

    /**
     * Checks if the name of a variable is compatible to the iso C standard. See 6.4.2 of the iso standard
     *
     * @param name name to check
     * @return <code>true</code> if valid, <code>false</code> if not
     */
    def isValidName(name: String): Boolean =
        (name.matches(REGEX_VALID_IDENTIFIER)
            && !name.startsWith("__")
            && !isReservedLanguageKeyword(name)
            && !isSystemLinkedName(name))

    def isSystemLinkedName(name: String) = SystemLinker.allLibs.par.contains(name)

    def isValidInProgram(name: Opt[String], morpheus: Morpheus): Boolean =
      ! (morpheus.getModuleInterface != null &&
        morpheus.getModuleInterface.isListed(name, morpheus.getFM))

    /**
     * Checks if the id valid for renaming
     */
    def isValidIdForRename(id: Id, morpheus : Morpheus,
                           validIdForRenameInCaseStudy: (Id, Morpheus) => Boolean = (i, m) => true): Boolean = {
        val blackListed =
            if (morpheus.getModuleInterface != null)
                !morpheus.getModuleInterface.isBlackListed(id.name)
            else true

        !(id.name == "main") && validIdForRenameInCaseStudy(id, morpheus) && !isSystemLinkedName(id.name) &&
            blackListed && id.hasPosition && !hasConflictingLinking(id, morpheus)
    }


    def isWritable(id: Id, morpheus : Morpheus) : Boolean = {
        val path =
            if (hasSameFileName(id, morpheus)) morpheus.getFile
            else id.getFile.get.replaceFirst("file ", "")
        new File(path).canWrite
    }

    /**
     * Checks if the name is a language keyword.
     *
     * @param name the name to check
     * @return <code>true</code> if language keyword
     */
    def isReservedLanguageKeyword(name: String) = LANGUAGE_KEYWORDS.contains(name)


    // Check whether a symbol is valid in the current file (module).
    def isValidInModule(name: String, element: AST, morpheus: Morpheus): Boolean = {

        // get the scope of the AST element
        // It is either one CompoundStatement or we take the last external definition
        // of this module.
        val scope = findPriorASTElem[CompoundStatement](element, morpheus.getASTEnv) match {
            case Some(x) => x.innerStatements.last.entry
            case _ => morpheus.getTranslationUnit.defs.last.entry
        }

        // Depending on the type the name may be defined in one of the following three
        // environments: variable, struct or union, enum.
        val env = morpheus.getEnv(scope).asInstanceOf[Env]
        val ctx = morpheus.getASTEnv.featureExpr(element)

        ! (hasConflictingType(env.varEnv(name), ctx, morpheus)
            || hasConflictingStructOrUnion(name, env)
            || hasConflictingType(env.typedefEnv(name), ctx, morpheus))
    }

    def comparePosition(e1: AST, e2: AST) = e1.getPositionFrom < e2.getPositionFrom

    // determine if a conflicting type is available in configuration ctx
    private def hasConflictingType(env: Conditional[CType], ctx: FeatureExpr, morpheus: Morpheus): Boolean = {
        ! ConditionalLib.items(env).forall {
            case (typeCtx, cType) => cType.isUnknown || (ctx and typeCtx isContradiction morpheus.getFM)
        }
    }

    private def hasConflictingStructOrUnion(name: String, env: Env): Boolean = {
        env.structEnv.someDefinition(name, false) || env.structEnv.someDefinition(name, true)
    }

    /**
     * Checks if the local linking information are also globally visible
     */
    private def hasConflictingLinking(id: Id, morpheus : Morpheus) = {
        val exports = morpheus.getTypeSystem.getInferredInterface().exports
        val imports = morpheus.getTypeSystem.getInferredInterface().imports

        val local = exports.exists(_.name == id.name) ||
            imports.exists(_.name == id.name)

        val global =
            if (morpheus.getModuleInterface == null)
                false
            else
                morpheus.getModuleInterface.nameIsListed(id.name)

        /** Checks if an identifier is declared with the specifier 'extern' but not listed in the linking interface */
        val externSpecifier = morpheus.getReferences(id).par.exists(ref =>
            findPriorASTElem[Declaration](ref.entry, morpheus.getASTEnv) match {
                case Some(d) => d.declSpecs.exists(_.entry match {
                    case _ : ExternSpecifier => true
                    case _ => false
                })
                case None => false
            })

        (local && !global) || (externSpecifier && !global)
    }

    private def hasSameFileName(id : Id, morpheus : Morpheus) : Boolean = {
        val entry = id.getFile.get.replaceFirst("file ", "")
        entry.equalsIgnoreCase(morpheus.getFile) ||
            getFileName(entry).equalsIgnoreCase(getFileName(morpheus.getFile))
    }

    private def getFileName(originalFilePath: String) =
        if (originalFilePath.contains(File.separatorChar))
            originalFilePath.substring(originalFilePath.lastIndexOf(File.separatorChar),
                originalFilePath.length).replace("/", "")
        else originalFilePath


}

case class RefactorException(error: String) extends Exception(error)

