package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.options.{RefactorType, FrontendOptions, OptionException, FrontendOptionsWithConfigFiles}
import de.fosd.typechef.{lexer, ErrorXML}
import java.io._
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.crefactor.evaluation.{Refactor, StatsJar}
import de.fosd.typechef.crefactor.evaluation.setup.{CLinking, Building, BuildCondition}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.crefactor.frontend.Editor
import javax.swing.SwingUtilities
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.SQLiteRefactor
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.BusyBoxRefactor
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.OpenSSLRefactor

object CRefactorFrontend extends App with InterfaceWriter with BuildCondition with Logging {

    private var command: Array[String] = Array()

    private var runOpt: FrontendOptions = new FrontendOptions()

    override def main(args: Array[String]): Unit = parse(args, true)

    def parse(args: Array[String], saveArg: Boolean = false) = {
        // Parsing MorphFrontend is adapted by the original typechef frontend
        runOpt = new FrontendOptionsWithConfigFiles()
        try {
            runOpt.parseOptions(args)
        } catch {
            case o: OptionException =>
                println("Invocation error: " + o.getMessage)
                println("use parameter --help for more information.")
                System.exit(-1)
        }

        // Current re-run hack - storing the initial arguments for parsing further files then the initial with the same arguments
        if (saveArg) command = args.foldLeft(List[String]())((args, arg) => {
            if (arg.equalsIgnoreCase(runOpt.getFile)) args
            else if (arg.equalsIgnoreCase("--refEval") || arg.equalsIgnoreCase("rename") || arg.equalsIgnoreCase("extract") || arg.equalsIgnoreCase("inline")) args
            else args :+ arg
        }).toArray

        processFile(runOpt)
    }

    def parseOrLoadAST(file: String): (AST, FeatureModel) = {
        val opt = new FrontendOptionsWithConfigFiles()
        opt.parseOptions(file +: command)

        val fm = getFM(opt)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        val ast = {
            if (runOpt.reuseAST && new File(opt.getSerializedASTFilename).exists()) loadSerializedAST(opt.getSerializedASTFilename)
            else parseAST(fm, opt)
        }

        if (ast == null) {
            logger.error("... failed reading AST " + opt.getFile + "\nExiting.")
            System.exit(-1)
        }

        (ast, fm)
    }

    private def processFile(opt: FrontendOptions) = {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val fm = getFM(opt)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        if (opt.writeBuildCondition) writeBuildCondition(opt.getFile)

        val linkInf = {
            if (opt.refLink) new CLinking(opt.getLinkingInterfaceFile)
            else null
        }

        if (opt.parse) {

            val ast = {
                if (runOpt.reuseAST && new File(opt.getSerializedASTFilename).exists()) loadSerializedAST(opt.getSerializedASTFilename)
                else parseAST(fm, opt)
            }

            if (ast == null) {
                errorXML.write()
                logger.error("... failed reading AST " + opt.getFile + "\nExiting.")
                System.exit(-1)
            }

            if (opt.serializeAST) serializeAST(ast, opt.getSerializedASTFilename)

            if (opt.writeInterface) writeInterface(ast, fm, opt, errorXML)

            if (opt.refEval) refactorEval(opt, ast, fm, linkInf)

            if (opt.prettyPrint) prettyPrint(ast, opt)

            if (opt.canBuild) testBuildingAndTesting(ast, fm, opt)

            if (opt.showGui) createAndShowGui(ast, fm, opt)
        }
    }

    private def getFM(opt: FrontendOptions): FeatureModel = {
        val fm = {
            if (opt.getUseDefaultPC) opt.getLexerFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
            else opt.getLexerFeatureModel.and(opt.getLocalFeatureModel)
        }

        if (opt.getUseDefaultPC && !opt.getFilePresenceCondition.isSatisfiable(fm)) {
            logger.error("file has contradictory presence condition. exiting.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            System.exit(-1)
        }

        fm
    }

    private def writeInterface(ast: AST, fm: FeatureModel, opt: FrontendOptions, errorXML: ErrorXML) {
        val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm, opt) with CTypeCache with CDeclUse
        ts.checkAST()
        ts.errors.map(errorXML.renderTypeError)

        val interface = {
            if (opt.getUseDefaultPC) ts.getInferredInterface().and(opt.getFilePresenceCondition)
            else ts.getInferredInterface()
        }
        ts.writeInterface(interface, new File(opt.getInterfaceFilename))
        if (opt.writeDebugInterface)
            ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
    }
    private def parseAST(fm: FeatureModel, opt: FrontendOptions): AST = {
        val parsingTime = new StopClock
        val parserMain = new ParserMain(new CParser(fm))
        val ast = parserMain.parserMain(lex(opt), opt)
        StatsJar.addStat(opt.getFile, Parsing, parsingTime.getTime)

        ast
    }
    private def testBuildingAndTesting(ast: AST, fm: FeatureModel, opt: FrontendOptions) {
        val builder: Building = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.building.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("openssl")) de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.setup.Builder
            else null
        }

        val canBuild = builder.canBuild(ast, fm, opt.getFile)
        logger.info("Can build " + new File(opt.getFile).getName + " : " + canBuild)
    }
    private def refactorEval(opt: FrontendOptions, ast: AST, fm: FeatureModel, linkInf: CLinking) {
        val caseStudy: Refactor = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) BusyBoxRefactor
            else if (opt.getRefStudy.equalsIgnoreCase("openssl")) OpenSSLRefactor
            else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) SQLiteRefactor
            else null
        }

        opt.getRefactorType match {
            case RefactorType.RENAME => caseStudy.rename(ast, fm, opt.getFile, linkInf)
            case RefactorType.EXTRACT => caseStudy.extract(ast, fm, opt.getFile, linkInf)
            case RefactorType.INLINE => caseStudy.inline(ast, fm, opt.getFile, linkInf)
            case RefactorType.NONE => println("No refactor type defined")
        }
    }
    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = CLexer.prepareTokens(new lexer.Main().run(opt, opt.parse))

    private def serializeAST(ast: AST, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(ast)
        fw.close()
    }

    private def loadSerializedAST(filename: String): AST = {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { /*println(desc);*/ super.resolveClass(desc) }
        }
        val ast = fr.readObject().asInstanceOf[AST]
        fr.close()
        ast
    }

    private def prettyPrint(ast: AST, options: FrontendOptions) = {
        val filePath = options.getFile ++ ".pp"
        val file = new File(filePath)
        logger.info("Pretty printing to: " + file.getCanonicalPath)
        val prettyPrinted = PrettyPrinter.print(ast).replace("definedEx", "defined")
        val writer = new FileWriter(file, false)
        writer.write(addBuildCondition(filePath, prettyPrinted))
        writer.flush()
        writer.close()
    }

    private def createAndShowGui(ast: AST, fm: FeatureModel, opts: FrontendOptions) = {
        val morpheus = new Morpheus(ast, fm, opts.getFile)
        SwingUtilities.invokeLater(new Runnable {
            def run() {
                val editor = new Editor(morpheus)
                editor.loadFileInEditor(opts.getFile)
                editor.pack
                editor.setVisible(true)
            }
        })
    }
}
