package de.fosd.typechef.crefactor.evaluation

import java.io._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr, SingleFeatureExpr, FeatureModel}
import java.util.regex.Pattern
import scala.io.Source
import de.fosd.typechef.crefactor.{Morpheus, Logging}
import java.util.{TimerTask, Timer, IdentityHashMap}
import de.fosd.typechef.crefactor.evaluation.setup.BuildCondition
import de.fosd.typechef.typesystem.linker.SystemLinker
import de.fosd.typechef.conditional.Opt
import java.util

trait Evaluation extends Logging with BuildCondition with ASTNavigation with ConditionalNavigation {

    val evalName: String
    val caseStudyPath: String
    val completePath: String
    val filesToEval: String
    val evalFiles: List[String]
    val blackListFiles: List[String]
    val blackListIds: List[String]
    val sourcePath: String
    val testPath: String
    val result: String

    val filterFeatures: List[String]
    val allFeaturesFile: String
    val allFeatures: (List[SingleFeatureExpr], IdentityHashMap[String, String])
    val pairWiseFeaturesFile: String
    val existingConfigsDir: String

    val featureModel: String
    val featureModel_DIMACS: String

    val runTimeout: Int

    val FORCE_VARIABILITY: Boolean
    val FORCE_LINKING: Boolean

    val maxConfigs: Int = 50

    def copyFile(file1: File, file2: File) = new FileOutputStream(file2).getChannel.transferFrom(new FileInputStream(file1).getChannel, 0, Long.MaxValue)

    /**
     * Runs a shell script with either default timeout or a custom timeout in ms
     */
    def runScript(script: String, dir: String, timeout: Int = runTimeout): (InputStream, InputStream) = {
        runScript(script, dir, null, timeout)
    }

    /**
     * Runs a shell script with either default timeout or a custom timeout in ms
     */
    def runScript(script: String, dir: String, args: String, timeout: Int): (InputStream, InputStream) = {
        val pb = if (args == null) new ProcessBuilder(script)
        else new ProcessBuilder(script, args)
        pb.directory(new File(dir))
        println(pb.command())
        val process = pb.start()
        val timer = new Timer(true)
        timer.schedule(new TimerTask() {
            def run() {
                process.destroy();
            }
        }, timeout)
        process.waitFor()
        (process.getInputStream, process.getErrorStream)
    }


    def writeRunResult(run: Int, morpheus: Morpheus, linkedFiles: List[(String, TranslationUnit)]) = {
        val runDir = new File(getResultDir(morpheus.getFile).getCanonicalPath + File.separatorChar + run + File.separatorChar)
        if (!runDir.exists) runDir.mkdirs()

        val path = runDir.getCanonicalPath + File.separatorChar + getFileName(morpheus.getFile)
        writePrettyPrintedTUnit(morpheus.getTranslationUnit, path)
        writePlainTUnit(morpheus.getTranslationUnit, path + ".tunit_plain")

        linkedFiles.foreach(file => {
            val linkedPath = runDir.getCanonicalPath + File.separatorChar + getFileName(file._1)
            writePrettyPrintedTUnit(file._2, linkedPath)
            writePlainTUnit(file._2, linkedPath + ".tunit_plain")
        })
    }


    def write(ast: AST, filePath: String, orgFile: String = null, overWriteOrgFile: Boolean = true) = {
        val refFile = if (orgFile != null) orgFile else filePath
        if (overWriteOrgFile) writePrettyPrintedTUnit(ast, filePath)
        val resultDir = getResultDir(refFile)
        val path = resultDir.getCanonicalPath + File.separatorChar + getFileName(filePath)
        writePrettyPrintedTUnit(ast, path)
        writePlainTUnit(ast, path + ".tunit_plain")
    }

    def writeResult(result: String, file: String) = {
        var out: FileWriter = null
        if (file.startsWith(".")) out = new java.io.FileWriter(file.replaceFirst(".", ""))
        else out = new java.io.FileWriter(file)

        out.write(result)
        out.flush()
        out.close()
    }

    def writePrettyPrintedTUnit(ast: AST, filePath: String) {
        val path = removeFilePrefix(filePath)
        logger.info("Pretty printing to: " + filePath)
        val file = new File(path)
        val prettyPrinted = PrettyPrinter.print(ast).replace("definedEx", "defined")
        val writer = new FileWriter(file, false)
        writer.write(addBuildCondition(path, prettyPrinted))
        writer.flush()
        writer.close()
    }

    def writePlainTUnit(ast: AST, filePath: String) {
        val writer = new FileWriter(filePath)
        writer.write(ast.toString)
        writer.flush()
        writer.close()
    }

    def writeError(error: String, originalFilePath: String) = {
        val out = new java.io.FileWriter(originalFilePath + ".error")
        out.write(error)
        out.write("\n")
        out.flush()
        out.close()
    }

    def writeException(exception: String, originalFilePath: String) = {
        val dir = getResultDir(originalFilePath)
        val out = new java.io.FileWriter(dir.getCanonicalPath + File.separatorChar + getFileName(originalFilePath) + ".exception")
        out.write(exception)
        out.write("\n")

        out.flush()
        out.close()
    }

    def writeException(exception: String, originalFilePath: String, run: Int) = {
        val dir = getResultDir(originalFilePath, run)
        val out = new java.io.FileWriter(dir.getCanonicalPath + File.separatorChar + getFileName(originalFilePath) + ".exception")
        out.write(exception)
        out.write("\n")

        out.flush()
        out.close()
    }

    def writeStats(stats: List[Any], originalFilePath: String, run: Int) = {
        val dir = getResultDir(originalFilePath, run)
        val out = new java.io.FileWriter(dir.getCanonicalPath + File.separatorChar + getFileName(originalFilePath) + ".stats")
        stats.foreach(stat => {
            out.write(stat.toString)
            out.write("\n")
        })
        out.flush()
        out.close()
    }

    def removeFilePrefix(originalFilePath: String) =
        if (originalFilePath.startsWith("file")) originalFilePath.substring(5)
        else originalFilePath

    def getFileName(originalFilePath: String) =
        originalFilePath.substring(originalFilePath.lastIndexOf(File.separatorChar), originalFilePath.length).replace("/", "")

    def getResultDir(originalFilePath: String, run: Int): File = {
        val outputFilePath = originalFilePath.replace(evalName, "result")
        val result = new File(outputFilePath + File.separatorChar + run + File.separatorChar)
        if (!result.exists()) result.mkdirs()
        result
    }

    def getResultDir(originalFilePath: String): File = {
        val outputFilePath = originalFilePath.replace(evalName, "result")
        val result = new File(outputFilePath + File.separatorChar)
        if (!result.exists()) result.mkdirs()
        result
    }

    def getEvaluationFiles: List[String] = {
        def readIn(reader: BufferedReader): List[String] = {
            reader.readLine() match {
                case null => List()
                case x => List(getFileName(x + ".c")).:::(readIn(reader))
            }
        }
        val reader = new BufferedReader(new FileReader(filesToEval))
        val files = readIn(reader)
        reader.close()
        files
    }

    def getEnabledFeaturesFromConfigFile(fm: FeatureModel, file: File): List[SingleFeatureExpr] = {
        val correctFeatureModelIncompatibility = false
        var ignoredFeatures = 0
        var changedAssignment = 0
        var totalFeatures = 0
        var fileEx: FeatureExpr = FeatureExprFactory.True
        var trueFeatures: Set[SingleFeatureExpr] = Set()
        var falseFeatures: Set[SingleFeatureExpr] = Set()

        val enabledPattern: Pattern = java.util.regex.Pattern.compile("([^=]*)=y")
        val disabledPattern: Pattern = java.util.regex.Pattern.compile("([^=]*)=n")
        for (line <- Source.fromFile(file).getLines().filterNot(_.startsWith("#")).filterNot(_.isEmpty)) {
            totalFeatures += 1
            var matcher = enabledPattern.matcher(line)
            if (matcher.matches()) {
                val name = matcher.group(1)
                val feature = FeatureExprFactory.createDefinedExternal(name)
                var fileExTmp = fileEx.and(feature)
                if (correctFeatureModelIncompatibility) {
                    val isSat = fileExTmp.isSatisfiable(fm)
                    logger.info(name + " " + (if (isSat) "sat" else "!sat"))
                    if (!isSat) {
                        fileExTmp = fileEx.andNot(feature)
                        logger.info("disabling feature " + feature)
                        //fileExTmp = fileEx; println("ignoring Feature " +feature)
                        falseFeatures += feature
                        changedAssignment += 1
                    } else {
                        trueFeatures += feature
                    }
                } else {
                    trueFeatures += feature
                }
                fileEx = fileExTmp
            } else {
                matcher = disabledPattern.matcher(line)
                if (matcher.matches()) {
                    val name = matcher.group(1)
                    val feature = FeatureExprFactory.createDefinedExternal(name)
                    var fileExTmp = fileEx.andNot(feature)
                    if (correctFeatureModelIncompatibility) {
                        val isSat = fileEx.isSatisfiable(fm)
                        println("! " + name + " " + (if (isSat) "sat" else "!sat"))
                        if (!isSat) {
                            fileExTmp = fileEx.and(feature)
                            logger.info("SETTING " + name + "=y")
                            trueFeatures += feature
                            changedAssignment += 1
                        } else {
                            falseFeatures += feature
                        }
                    } else {
                        falseFeatures += feature
                    }
                    fileEx = fileExTmp
                } else {
                    ignoredFeatures += 1
                    logger.info("ignoring line: " + line)
                }
            }
        }
        trueFeatures.par.filterNot(ft => filterFeatures.contains(ft.feature)).toList
    }

    def getAllFeaturesFromConfigFile(fm: FeatureModel, file: File): (List[SingleFeatureExpr], IdentityHashMap[String, String]) = {
        val correctFeatureModelIncompatibility = false
        var ignoredFeatures = 0
        var changedAssignment = 0
        var totalFeatures = 0
        var fileEx: FeatureExpr = FeatureExprFactory.True
        var trueFeatures: Set[SingleFeatureExpr] = Set()
        var falseFeatures: Set[SingleFeatureExpr] = Set()
        val assignValues = new IdentityHashMap[String, String]()

        val enabledPattern: Pattern = java.util.regex.Pattern.compile("([^=]*)=.*")
        val disabledPattern: Pattern = java.util.regex.Pattern.compile("([^=]*) is*")
        for (line <- Source.fromFile(file).getLines().filterNot(_.startsWith("#")).filterNot(_.isEmpty)) {
            totalFeatures += 1
            var matcher = enabledPattern.matcher(line)
            if (matcher.matches()) {
                val name = matcher.group(1)
                val value = line.substring(line.lastIndexOf('=') + 1).trim
                val feature = FeatureExprFactory.createDefinedExternal(name)
                if (!value.equals("y")) assignValues.put(feature.feature, value)
                var fileExTmp = fileEx.and(feature)
                if (correctFeatureModelIncompatibility) {
                    val isSat = fileExTmp.isSatisfiable(fm)
                    logger.info(name + " " + (if (isSat) "sat" else "!sat"))
                    if (!isSat) {
                        fileExTmp = fileEx.andNot(feature)
                        logger.info("disabling feature " + feature)
                        //fileExTmp = fileEx; println("ignoring Feature " +feature)
                        falseFeatures += feature
                        changedAssignment += 1
                    } else {
                        trueFeatures += feature
                    }
                } else {
                    trueFeatures += feature
                }
                fileEx = fileExTmp
            } else {
                matcher = disabledPattern.matcher(line)
                if (matcher.matches()) {
                    val name = matcher.group(1)
                    val feature = FeatureExprFactory.createDefinedExternal(name)
                    var fileExTmp = fileEx.andNot(feature)
                    if (correctFeatureModelIncompatibility) {
                        val isSat = fileEx.isSatisfiable(fm)
                        println("! " + name + " " + (if (isSat) "sat" else "!sat"))
                        if (!isSat) {
                            fileExTmp = fileEx.and(feature)
                            logger.info("SETTING " + name + "=y")
                            trueFeatures += feature
                            changedAssignment += 1
                        } else {
                            falseFeatures += feature
                        }
                    } else {
                        falseFeatures += feature
                    }
                    fileEx = fileExTmp
                } else {
                    ignoredFeatures += 1
                    logger.info("ignoring line: " + line)
                }
            }
        }
        ((trueFeatures ++ falseFeatures).toList, assignValues)
    }

    def constantSlice[T](list: List[T], start: Int, end: Int) = list.drop(start).take(end - start)

    def evaluateScriptResult(result: (InputStream, InputStream)): (Boolean, String, String) = {
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

    def streamsToString(streams: (InputStream, InputStream)): (String, String) = {
        val readerOut = new BufferedReader(new InputStreamReader(streams._1))
        val readerErr = new BufferedReader(new InputStreamReader(streams._2))

        val out = readIn(readerOut, new StringBuilder)
        val err = readIn(readerErr, new StringBuilder)
        (out, err)
    }

    def readIn(reader: BufferedReader, builder: StringBuilder): String = {
        while (reader.ready()) {
            val line = reader.readLine()
            builder.append(line + "\n")
        }
        builder.toString()
    }

    def addOwnGCCcmds(arg: String, line: String, enabledFeatures: Set[String], disabledFeatures: Set[String]): String = {
        val args = line.substring(arg.length).split(" ")

        val dFeatures = enabledFeatures.filterNot(eFeature => args.exists(_.endsWith(eFeature)))
        val filterArgs = args.toList.filterNot(argFeature => disabledFeatures.exists(argFeature.endsWith))

        arg + " " + filterArgs + dFeatures.map(feature => "-D" + feature).mkString(" ")
    }

    def isSystemLinkedName(name: String) = SystemLinker.allLibs.par.contains(name)

    def isExternalDeclWithNoLinkingInformation(id: Id, morpheus: Morpheus): Boolean = {
        if ((morpheus.getModuleInterface != null)
            && morpheus.getModuleInterface.isListed(Opt(parentOpt(id, morpheus.getASTEnv).feature, id.name), morpheus.getFM))
            false
        else
            morpheus.getDecls(id).exists(
                findPriorASTElem[Declaration](_, morpheus.getASTEnv) match {
                    case Some(entry) => entry.declSpecs.exists(_.entry match {
                        case ExternSpecifier() =>
                            logger.info("Is external declared and linked variable")
                            true
                        case _ => false
                    })
                    case _ => false
                })
    }

    def getAllFeaturesFromUniqueFeatureFile =
        (Source.fromFile(allFeaturesFile).getLines().map(FeatureExprFactory.createDefinedExternal).toList, new java.util.IdentityHashMap[String, String]())
}