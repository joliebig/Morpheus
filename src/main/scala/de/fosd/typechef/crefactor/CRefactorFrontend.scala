package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.options.{RefactorType, FrontendOptions, OptionException, FrontendOptionsWithConfigFiles}
import de.fosd.typechef.{VALexer, lexer}
import java.io._
import de.fosd.typechef.parser.TokenReader
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.typesystem.linker.InterfaceWriter
import de.fosd.typechef.crefactor.evaluation.{PreparedRefactorings, Evaluation, Refactor, StatsCan}
import de.fosd.typechef.crefactor.evaluation.setup.{CModuleInterfaceGenerator, Building, BuildCondition}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import de.fosd.typechef.parser.c.CTypeContext
import de.fosd.typechef.typesystem.{CDeclUse, CTypeCache, CTypeSystemFrontend}
import de.fosd.typechef.crefactor.frontend.Editor
import javax.swing.SwingUtilities
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.SQLiteRefactorEvaluation
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.BusyBoxRefactorEvaluation
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.OpenSSLRefactorEvaluation
import de.fosd.typechef.crefactor.backend.CModuleInterface
import de.fosd.typechef.featureexpr.bdd.FeatureExprHelper

object CRefactorFrontend extends App with InterfaceWriter with BuildCondition with Logging with EnforceTreeHelper {

    private var command: Array[String] = Array()

    private var runOpt: FrontendOptions = new FrontendOptions()

    override def main(args: Array[String]): Unit = {
        runOpt = new FrontendOptionsWithConfigFiles()

        try {
            runOpt.parseOptions(args)
        } catch {
            case o: OptionException => printInvokationErrorAndExit(o.getMessage)
        }

        if (runOpt.writeProjectInterface) writeProjectModuleInterface(runOpt)

        else if (runOpt.parse) parseOrLoadTUnitandProcess(args, true)


        logger.info("# unique Sat calls: " + FeatureExprHelper.uniqueSatCalls)
        logger.info("# cached Sat calls: " + FeatureExprHelper.cachedSatCalls)
        logger.info("# all Sat calls: " + (FeatureExprHelper.uniqueSatCalls + FeatureExprHelper.cachedSatCalls))
    }

    def parseOrLoadTUnitandProcess(args: Array[String], saveArg: Boolean = false) = {
        // Current re-run hack - storing the initial arguments for parsing further files then the initial with the same arguments
        if (saveArg) command = args.foldLeft(List[String]())((args, arg) => {
            if (arg.equalsIgnoreCase(runOpt.getFile)) args
            else if (arg.equalsIgnoreCase("--refEval") || arg.equalsIgnoreCase("rename") || arg.equalsIgnoreCase("extract") || arg.equalsIgnoreCase("inline")) args
            else args :+ arg
        }).toArray

        val fm = getFM(runOpt)
        runOpt.setFullFeatureModel(fm)

        val tunit = getTunit(runOpt, fm)

        if (tunit == null) {
            logger.error("... failed reading AST " + runOpt.getFile + "\nExiting.")
            System.exit(-1)
        }

        processFile(tunit, fm, runOpt)
    }

    def getTUnit(toLoad: String): (TranslationUnit, FeatureModel) = {
        val file = {
            if (toLoad.startsWith("file")) toLoad.substring(5)
            else toLoad
        }
        logger.info("Loading file: " + file)
        val opt = new FrontendOptions()
        opt.parseOptions(file +: command.clone())

        val fm = getFM(opt)
        opt.setFullFeatureModel(fm) //otherwise the lexer does not get the updated feature model with file presence conditions

        val tunit = getTunit(opt, fm)

        if (tunit == null) {
            logger.error("... failed reading TUnit " + opt.getFile + "\nExiting.")
            System.exit(-1)
        }

        (tunit, fm)
    }


    private def getTunit(opt: FrontendOptions, fm: FeatureModel): TranslationUnit = {
        val tunit =
            if (runOpt.reuseAST && new File(opt.getSerializedTUnitFilename).exists())
                loadSerializedTUnit(opt.getSerializedTUnitFilename)
            else parseTUnit(fm, opt)
        prepareAST[TranslationUnit](tunit)
    }
    private def writeProjectModuleInterface(options: FrontendOptions) = {
        val linker: CModuleInterfaceGenerator = {
            if (options.getRefStudy.equalsIgnoreCase("busybox"))
                de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.linking.BusyBoxModuleInterfaceGenerator
            else if (options.getRefStudy.equalsIgnoreCase("openssl"))
                de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup.OpenSSLLinkModuleInterfaceGenerator
            else if (options.getRefStudy.equalsIgnoreCase("sqlite"))
                de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.setup.SQLiteModuleInterfaceGenerator
            else null
        }

        linker.main(Array())
    }

    private def processFile(tunit: TranslationUnit, fm: FeatureModel, opt: FrontendOptions) = {
        val linkInf = {
            if (opt.refLink) new CModuleInterface(opt.getLinkingInterfaceFile)
            else null
        }

        if (opt.writeBuildCondition) writeBuildCondition(opt.getFile)

        if (opt.serializeAST) serializeTUnit(tunit, opt.getSerializedTUnitFilename)

        if (opt.writeInterface) writeInterface(tunit, fm, opt)

        if (opt.prepareRef) prepareRefactor(opt, tunit, fm, linkInf)

        if (opt.refEval) evaluateRefactor(opt, tunit, fm, linkInf)

        if (opt.prettyPrint) prettyPrint(tunit, opt)

        if (opt.canBuild) testBuildingAndTesting(tunit, fm, opt)

        if (opt.showGui) createAndShowGui(tunit, fm, opt, linkInf)

    }

    private def getFM(opt: FrontendOptions): FeatureModel = {
        // note: we use the full feature model instead of the small one
        val fm = {
            if (opt.getUseDefaultPC) opt.getFullFeatureModel.and(opt.getLocalFeatureModel).and(opt.getFilePresenceCondition)
            else opt.getFullFeatureModel.and(opt.getLocalFeatureModel)
        }

        if (opt.getUseDefaultPC && !opt.getFilePresenceCondition.isSatisfiable(fm)) {
            logger.error("file has contradictory presence condition. exiting.") //otherwise this can lead to strange parser errors, because True is satisfiable, but anything else isn't
            System.exit(-1)
        }

        fm
    }

    private def writeInterface(tunit: TranslationUnit, fm: FeatureModel, opt: FrontendOptions) {
        val ts = new CTypeSystemFrontend(tunit, fm, opt) with CTypeCache with CDeclUse
        ts.checkAST()

        val interface = {
            if (opt.getUseDefaultPC) ts.getInferredInterface().and(opt.getFilePresenceCondition)
            else ts.getInferredInterface()
        }
        ts.writeInterface(interface, new File(opt.getInterfaceFilename))
        if (opt.writeDebugInterface)
            ts.debugInterface(interface, new File(opt.getDebugInterfaceFilename))
    }
    private def parseTUnit(fm: FeatureModel, opt: FrontendOptions): TranslationUnit = {
        val parsingTime = new StopClock
        val parserMain = new ParserMain(new CParser(fm))
        val tUnit = parserMain.parserMain(lex(opt), opt, fm)

        StatsCan.addStat(opt.getFile, Parsing, parsingTime.getTime)

        tUnit
    }
    private def testBuildingAndTesting(tunit: TranslationUnit, fm: FeatureModel, opt: FrontendOptions) {
        val builder: Building = {
            if (opt.getRefStudy.equalsIgnoreCase("busybox")) de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.building.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("openssl")) de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup.Builder
            else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.setup.Builder
            else null
        }

        val canBuild = builder.canBuild(tunit, fm, opt.getFile)
        logger.info("Can build " + new File(opt.getFile).getName + " : " + canBuild)
    }
    private def prepareRefactor(opt: FrontendOptions, tunit: TranslationUnit,
                                 fm: FeatureModel, linkInf: CModuleInterface)  {
        val caseStudy: Refactor = getRefactorStudy(opt)
        val prepared = caseStudy.prepareForEvaluation(tunit, fm, opt.getFile, linkInf)
        serializePreparedRefactorings(prepared, opt.getPreparedRefactoringsFileName)
    }


    private def evaluateRefactor(opt: FrontendOptions, tunit: TranslationUnit,
                             fm: FeatureModel, linkInf: CModuleInterface) {
        val caseStudy: Refactor = getRefactorStudy(opt)

        val preparedRefactor = loadPreparedRefactorings(opt.getPreparedRefactoringsFileName)

        opt.getRefactorType match {
            case RefactorType.RENAME => caseStudy.rename(preparedRefactor, tunit, fm, opt.getFile, linkInf)
            case RefactorType.EXTRACT => caseStudy.extract(preparedRefactor, tunit, fm, opt.getFile, linkInf)
            case RefactorType.INLINE => caseStudy.inline(preparedRefactor, tunit, fm, opt.getFile, linkInf)
            case RefactorType.NONE => println("No engine type defined")
        }
    }
    private def lex(opt: FrontendOptions): TokenReader[CToken, CTypeContext] = {
        val tokens = new lexer.LexerFrontend().run(opt, opt.parse)
        val in = CLexerAdapter.prepareTokens(tokens)
        in
    }
    private def getRefactorStudy(opt: FrontendOptions): Evaluation with Refactor =
        if (opt.getRefStudy.equalsIgnoreCase("busybox")) BusyBoxRefactorEvaluation
        else if (opt.getRefStudy.equalsIgnoreCase("openssl")) OpenSSLRefactorEvaluation
        else if (opt.getRefStudy.equalsIgnoreCase("sqlite")) SQLiteRefactorEvaluation
        else null

    private def serializeTUnit(tUnit: TranslationUnit, filename: String) =
        serializeFile(tUnit, filename)
    
    private def serializePreparedRefactorings(prepared : PreparedRefactorings, filename: String) =
        serializeFile(prepared, filename)

    private def loadSerializedTUnit(filename: String): TranslationUnit =
        loadSerializedGZipFile[TranslationUnit](filename)

    private def loadPreparedRefactorings(filename: String) : PreparedRefactorings =
        loadSerializedGZipFile[PreparedRefactorings](filename)
    
    private def serializeFile(obj: AnyRef, filename: String) {
        val fw = new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
        fw.writeObject(obj)
        fw.close()
    }

    private def loadSerializedGZipFile[T](filename : String) : T = {
        val fr = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename))) {
            override protected def resolveClass(desc: ObjectStreamClass) = { super.resolveClass(desc) }
        }
        val loaded = fr.readObject().asInstanceOf[T]
        fr.close()

        loaded
    }

    private def prettyPrint(tunit: TranslationUnit, options: FrontendOptions) = {
        val filePath = options.getFile ++ ".pp"
        val file = new File(filePath)
        logger.info("Pretty printing to: " + file.getCanonicalPath)
        val prettyPrinted = PrettyPrinter.print(tunit).replace("definedEx", "defined")
        val writer = new FileWriter(file, false)
        writer.write(addBuildCondition(filePath, prettyPrinted))
        writer.flush()
        writer.close()
    }

    private def createAndShowGui(tunit: TranslationUnit, fm: FeatureModel, opts: FrontendOptions, linkInf: CModuleInterface) = {
        val morpheus = new Morpheus(tunit, fm, linkInf, opts.getFile)
        SwingUtilities.invokeLater(new Runnable {
            def run() {
                val editor = new Editor(morpheus)
                editor.loadFileInEditor(opts.getFile)
                editor.pack()
                editor.setVisible(true)
            }
        })
    }

    private def printInvokationErrorAndExit(message : String) = {
        println("Invocation error: " + message)
        println("use parameter --help for more information.")
        System.exit(-1)
    }
}
