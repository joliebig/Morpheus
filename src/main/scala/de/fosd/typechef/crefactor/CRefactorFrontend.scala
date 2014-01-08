package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.options.{RefactorType, FrontendOptions, OptionException, FrontendOptionsWithConfigFiles}
import de.fosd.typechef.{lexer, ErrorXML}
import java.io._
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.evaluation.{Refactor, StatsJar}
import de.fosd.typechef.crefactor.evaluation.setup.{Building, BuildCondition}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.crefactor.frontend.Editor
import javax.swing.SwingUtilities
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.SQLiteRefactor
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.BusyBoxRefactor

object CRefactorFrontend extends App with InterfaceWriter with BuildCondition {

    var command: Array[String] = Array()

    override def main(args: Array[String]): Unit = parse(args, true)

    def parse(file: String): (AST, FeatureModel) = parse(file +: command, false)

    def parse(args: Array[String], saveArg: Boolean): (AST, FeatureModel) = {
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

    private def processFile(opt: FrontendOptions): (AST, FeatureModel) = {
        val errorXML = new ErrorXML(opt.getErrorXMLFile)
        opt.setRenderParserError(errorXML.renderParserError)

        val fm = opt.getLexerFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
        opt.setFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        if (!opt.getFilePresenceCondition.isSatisfiable(fm)) {
            println("file has contradictory presence condition. existing.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            return (null, null)
        }

        // TODO Implement studies for refactorings

        var ast: AST = null

        if (opt.writeBuildCondition) writeBuildCondition(opt.getFile)

        val linkInf = if (opt.refLink) new CLinking(opt.getLinkingInterfaceFile)
        else null

        if (opt.reuseAST && opt.parse && new File(opt.getSerializedASTFilename).exists()) {
            ast = loadSerializedAST(opt.getSerializedASTFilename)
            if (ast == null) println("... failed reading AST\n")
        }

        if (opt.parse) {

            if (ast == null) ast = parseAST(fm, opt)

            if (ast == null) errorXML.write()

            if (ast != null && opt.serializeAST) serializeAST(ast, opt.getSerializedASTFilename)

            if (opt.writeInterface) writeInterface(ast, fm, opt, errorXML)

            if (opt.refEval) refactorEval(opt, ast, fm, linkInf)

            if (opt.prettyPrint) prettyPrint(ast, opt)

            if (opt.canBuild) canBuildAndTest(ast, opt)

            if (opt.showGui) createAndShowGui(ast, fm, opt)
        }



        (ast, fm)
    }


    private def writeInterface(ast: AST, fm: FeatureModel, opt: FrontendOptions, errorXML: ErrorXML) {
        val ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm, opt) with CTypeCache with CDeclUse
        val interface = ts.getInferredInterface().and(opt.getFilePresenceCondition)

        val typeCheckStatus = ts.checkAST()
        ts.errors.map(errorXML.renderTypeError)

        ts.writeInterface(interface, new File(opt.getInterfaceFilename))
        if (opt.writeDebugInterface)
            ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
    }
    private def parseAST(fm: FeatureModel, opt: FrontendOptions): AST = {
        val parsingTime = new TimeMeasurement
        val parserMain = new ParserMain(new CParser(fm))
        val ast = parserMain.parserMain(lex(opt), opt)
        StatsJar.addStat(opt.getFile, Parsing, parsingTime.getTime)

        ast
    }
    private def canBuildAndTest(ast: AST, opt: FrontendOptions) {
        val builder: Building = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.setup.building.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("openssl")) de.fosd.typechef.crefactor.evaluation.openSSL.setup.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) de.fosd.typechef.crefactor.evaluation.sqlite.setup.Builder
            else null
        }

        val canBuild = builder.canBuild(ast, opt.getFile)
        println("+++ Can build " + new File(opt.getFile).getName + " : " + canBuild + " +++")
    }
    private def refactorEval(opt: FrontendOptions, ast: AST, fm: FeatureModel, linkInf: CLinking) {
        val caseStudy: Refactor = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) BusyBoxRefactor
            // else if (opt.getRefStudy.equalsIgnoreCase("openssl")) de.fosd.typechef.crefactor.evaluation.openSSL.setup.Builder
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
        println("+++ Pretty printing to: " + file.getCanonicalPath)
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
