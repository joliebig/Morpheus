package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.options.{RefactorType, FrontendOptions, OptionException, FrontendOptionsWithConfigFiles}
import de.fosd.typechef.{lexer, ErrorXML}
import java.io.{ObjectStreamClass, FileInputStream, ObjectInputStream, File}
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.refactor.Rename
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.setup.building.Builder
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.evaluation.StatsJar

object CRefactorFrontend extends App with InterfaceWriter {

    var command: Array[String] = Array()

    override def main(args: Array[String]): Unit = parse(args, true)

    def parse(file: String): (AST, FeatureModel, CTypeSystemFrontend) = parse(file +: command, false)

    def parse(args: Array[String], saveArg: Boolean): (AST, FeatureModel, CTypeSystemFrontend) = {
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

        // Current re-run hack - storing the initial arguments for parsing further files then the initial with the same arguments
        if (saveArg) command = args.foldLeft(List[String]())((args, arg) => {
            if (arg.equalsIgnoreCase(opt.getFile)) args
            else if (arg.equalsIgnoreCase("--refEval") || arg.equalsIgnoreCase("rename") || arg.equalsIgnoreCase("extract") || arg.equalsIgnoreCase("inline")) args
            else args :+ arg
        }).toArray

        processFile(opt)
    }

    private def processFile(opt: FrontendOptions): (AST, FeatureModel, CTypeSystemFrontend) = {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val fm = opt.getLexerFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        if (!opt.getFilePresenceCondition.isSatisfiable(fm)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return (null, null, null)
        }

        var ast: AST = null
        var featureModel: FeatureModel = null
        var ts: CTypeSystemFrontend = null
        var linkInf: CLinking = null

        if (opt.reuseAST && opt.parse && new File(opt.getSerializedASTFilename).exists()) {
            ast = loadSerializedAST(opt.getSerializedASTFilename)
            if (ast == null) println("... failed reading AST\n")
        }

        if (opt.refLink) {
            linkInf = new CLinking(opt.getLinkingInterfaceFile)
        }

        if (opt.parse) {

            if (ast == null) {
                //no parsing and serialization if read serialized ast
                val parsingTime = new TimeMeasurement
                val parserMain = new ParserMain(new CParser(fm))
                ast = parserMain.parserMain(lex(opt), opt)
                StatsJar.addStat(opt.getFile, Parsing, parsingTime.getTime)
            }

            if (ast != null) featureModel = opt.getTypeSystemFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
            errorXML.write()

            /**
            if (opt.typecheck) {
                ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], featureModel, opt)
                ts.checkAST()
            }  */

            if (opt.refEval) {
                opt.getRefactorType match {
                    case RefactorType.RENAME => Rename.evaluate(ast, featureModel, opt.getFile, linkInf)
                    case RefactorType.EXTRACT => //Extract.evaluate(ast, featureModel, ts, opt.getFile, duration)
                    case RefactorType.INLINE => //Inline.evaluate(ast, featureModel, ts, opt.getFile, duration)
                    case RefactorType.NONE => println("No refactor type defined")
                }
            }
        }

        if (opt.canBuild) {
            val canBuild = Builder.canBuild(ast, opt.getFile)
            println("+++ Can build " + new File(opt.getFile).getName + " : " + canBuild + " +++")
        }

        (ast, featureModel, ts)
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
