package de.fosd.typechef.crefactor

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import java.util.Observable
import java.io._
import de.fosd.typechef.{lexer, ErrorXML}
import de.fosd.typechef.options.{FrontendOptions, OptionException, FrontendOptionsWithConfigFiles}
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.parser.c.TranslationUnit

class Morpheus(ast: AST, fm: FeatureModel, file: File) extends Observable with CDeclUse with CTypeEnv with CEnvCache with CTypeCache with CTypeSystem with Logging {
    def this(ast: AST) = this(ast, null, null)

    def this(ast: AST, fm: FeatureModel) = this(ast, fm, null)

    def this(ast: AST, file: File) = this(ast, null, file)

    private var astCached: AST = ast
    private var astEnvCached: ASTEnv = CASTEnv.createASTEnv(ast)
    typecheckTranslationUnit(ast.asInstanceOf[TranslationUnit])

    //private var ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm)
    //ts.checkAST
    def update(ast: AST) {
        astCached = ast
        astEnvCached = CASTEnv.createASTEnv(astCached)
        //ts = new CTypeSystemFrontend(astCached.asInstanceOf[TranslationUnit], fm)
        //ts.checkAST
        typecheckTranslationUnit(ast.asInstanceOf[TranslationUnit])
        setChanged()
        notifyObservers()
    }

    def getEnv(ast: AST) = lookupEnv(ast)

    def getAST = astCached

    def getASTEnv = astEnvCached

    def getFeatureModel = fm

    def getFile = file
}

object MorphFrontend {

    def getDefaultTypeChefArguments(file: String, systemProperties: String, includeHeader: String, includeDir: String, featureModel: String) =
        Array(file, "-c", systemProperties, "-x", "CONFIG_", "--include", includeHeader,
            "-I", includeDir, "--featureModelFExpr", featureModel, "--debugInterface", "--recordTiming", "--lexNoStdout",
            "--parserstatistics", "-U", "HAVE_LIBDMALLOC", "-DCONFIG_FIND", "-U", "CONFIG_FEATURE_WGET_LONG_OPTIONS",
            "-U", "ENABLE_NC_110_COMPAT", "-U", "CONFIG_EXTRA_COMPAT", "-D_GNU_SOURCE")

    def parse(file: String, systemProperties: String, includeHeader: String, includeDir: String, featureModel: String): (AST, FeatureModel) = {
        val args = getDefaultTypeChefArguments(file, systemProperties, includeHeader, includeDir, featureModel)
        parse(args)
    }

    def parse(args: Array[String]): (AST, FeatureModel) = {
        // Parsing MorphFrontend is adapted by the original typechef frontend
        val opt = new FrontendOptionsWithConfigFiles()

        try {
            opt.parseOptions(args)
        } catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                throw o
        }

        processFile(opt)
    }

    private def processFile(opt: FrontendOptions): (AST, FeatureModel) = {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val fm = opt.getLexerFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        if (!opt.getFilePresenceCondition.isSatisfiable(fm)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return (null, null)
        }

        var ast: AST = null
        var featureModel: FeatureModel = null
        if (opt.reuseAST && opt.parse && new File(opt.getSerializedASTFilename).exists()) {
            ast = loadSerializedAST(opt.getSerializedASTFilename)
            if (ast == null) println("... failed reading AST\n")
        }

        if (opt.parse) {
            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parserMain = new ParserMain(new CParser(fm))
                ast = parserMain.parserMain(lex(opt), opt)
            }

            if (ast != null) featureModel = opt.getTypeSystemFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
            errorXML.write()

            val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], featureModel, opt)
            val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

            println("+++ Interface Stats +++")
            println(opt.getFile)
            println("+++ Declared Features +++")
            println(interface.declaredFeatures)
            println("+++ Exports +++")
            println(interface.exports)
            println("+++ Imports +++")
            println(interface.imports)
            println("+++ Imported Features +++")
            println(interface.importedFeatures)

        }

        (ast, featureModel)
    }


    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = CLexer.prepareTokens(new lexer.Main().run(opt, opt.parse))

    private def loadSerializedAST(filename: String): AST = {
        val fr = new ObjectInputStream(new FileInputStream(filename)) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val ast = fr.readObject().asInstanceOf[AST]
        fr.close()
        ast
    }
}
