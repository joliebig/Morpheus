package de.fosd.typechef.crefactor.evaluation

import java.io._
import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr, SingleFeatureExpr, FeatureModel}
import java.util.regex.Pattern
import scala.io.Source
import de.fosd.typechef.crefactor.{Morpheus, Logging}
import java.util.{TimerTask, Timer, IdentityHashMap}
import scala.collection.immutable.HashMap
import de.fosd.typechef.crefactor.evaluation.setup.BuildCondition
import de.fosd.typechef.parser.c.GnuAsmExpr
import de.fosd.typechef.conditional.Choice
import de.fosd.typechef.parser.c.Id
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.typesystem.linker.SystemLinker

trait Evaluation extends Logging with BuildCondition with ASTNavigation with ConditionalNavigation {

    val evalName: String
    val caseStudyPath: String
    val completePath: String
    val filesToEval: String
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

    val maxConfigs: Int = 1000


    /**
     * The following class it not part of the default TypeChef Branch. In order to read in csv - configurations correctly
     * the following class is copied from https://github.com/ckaestne/TypeChef/blob/liveness/Sampling/src/main/scala/de/fosd/typechef/FamilyBasedVsSampleBased.scala
     */

    /** Maps SingleFeatureExpr Objects to IDs (IDs only known/used in this file) */
    var featureIDHashmap: Map[SingleFeatureExpr, Int] = null
    /** List of all features found in the currently processed file */
    var features: List[SingleFeatureExpr] = null

    // representation of a product configuration that can be dumped into a file
    // and loaded at further runs
    class SimpleConfiguration(private val config: scala.collection.immutable.BitSet) extends scala.Serializable {

        def this(trueSet: List[SingleFeatureExpr], falseSet: List[SingleFeatureExpr]) = this(
        {
            val ret: scala.collection.mutable.BitSet = scala.collection.mutable.BitSet()
            for (tf: SingleFeatureExpr <- trueSet) ret.add(featureIDHashmap(tf))
            for (ff: SingleFeatureExpr <- falseSet) ret.remove(featureIDHashmap(ff))
            ret.toImmutable
        }
        )

        def getTrueSet: Set[SingleFeatureExpr] = {
            features.filter({
                fex: SingleFeatureExpr => config.apply(featureIDHashmap(fex))
            }).toSet
        }

        def getFalseSet: Set[SingleFeatureExpr] = {
            features.filterNot({
                fex: SingleFeatureExpr => config.apply(featureIDHashmap(fex))
            }).toSet
        }

        override def toString: String = {
            features.map(
            {
                fex: SingleFeatureExpr => if (config.apply(featureIDHashmap(fex))) fex else fex.not()
            }
            ).mkString("&&")
        }

        // caching, values of this field will not be serialized
        @transient
        private var featureExpression: FeatureExpr = null

        def toFeatureExpr: FeatureExpr = {
            if (featureExpression == null)
                featureExpression = FeatureExprFactory.createFeatureExprFast(getTrueSet, getFalseSet)
            featureExpression
        }

        /**
         * This method assumes that all features in the parameter-set appear in either the trueList, or in the falseList
         * @param features given feature set
         * @return
         */
        def containsAllFeaturesAsEnabled(features: Set[SingleFeatureExpr]): Boolean = {
            for (fex <- features) {
                if (!config.apply(featureIDHashmap(fex))) return false
            }
            true
        }

        /**
         * This method assumes that all features in the parameter-set appear in the configuration (either as true or as false)
         * @param features given feature set
         * @return
         */
        def containsAllFeaturesAsDisabled(features: Set[SingleFeatureExpr]): Boolean = {
            for (fex <- features) {
                if (config.apply(featureIDHashmap(fex))) return false
            }
            true
        }

        def containsAtLeastOneFeatureAsEnabled(set: Set[SingleFeatureExpr]): Boolean =
            !containsAllFeaturesAsDisabled(set)

        def containsAtLeastOneFeatureAsDisabled(set: Set[SingleFeatureExpr]): Boolean =
            !containsAllFeaturesAsEnabled(set)

        override def equals(other: Any): Boolean = {
            if (!other.isInstanceOf[SimpleConfiguration]) super.equals(other)
            else {
                val otherSC = other.asInstanceOf[SimpleConfiguration]
                otherSC.config.equals(this.config)
            }
        }

        override def hashCode(): Int = config.hashCode()
    }

    def initializeFeatureList(family_ast: AST) {
        features = getAllFeatures(family_ast)
        featureIDHashmap = new HashMap[SingleFeatureExpr, Int]().++(features.zipWithIndex)
    }

    /**
     * Returns a sorted list of all features in this AST, including Opt and Choice Nodes
     * @param root input element
     * @return
     */
    def getAllFeatures(root: Product): List[SingleFeatureExpr] = {
        var featuresSorted: List[SingleFeatureExpr] = getAllFeaturesRec(root).toList
        // sort to eliminate any non-determinism caused by the set
        featuresSorted = featuresSorted.sortWith({
            (x: SingleFeatureExpr, y: SingleFeatureExpr) => x.feature.compare(y.feature) > 0
        })
        featuresSorted //.map({s:String => FeatureExprFactory.createDefinedExternal(s)});
    }

    private def getAllFeaturesRec(root: Any): Set[SingleFeatureExpr] = {
        root match {
            case x: Opt[_] => x.feature.collectDistinctFeatureObjects.toSet ++ getAllFeaturesRec(x.entry)
            case x: Choice[_] => x.feature.collectDistinctFeatureObjects.toSet ++ getAllFeaturesRec(x.thenBranch) ++ getAllFeaturesRec(x.elseBranch)
            case l: List[_] => {
                var ret: Set[SingleFeatureExpr] = Set()
                for (x <- l) {
                    ret = ret ++ getAllFeaturesRec(x)
                }
                ret
            }
            case x: Product => {
                var ret: Set[SingleFeatureExpr] = Set()
                for (y <- x.productIterator.toList) {
                    ret = ret ++ getAllFeaturesRec(y)
                }
                ret
            }
            case o => Set()
        }
    }

    def loadConfigurationsFromCSVFile(csvFile: File, dimacsFile: File, features: List[SingleFeatureExpr], fm: FeatureModel, fnamePrefix: String = ""): (List[SimpleConfiguration], String) = {
        var retList: List[SimpleConfiguration] = List()

        // determine the feature ids used by the sat solver from the dimacs file
        // dimacs format (c stands for comment) is "c 3779 AT76C50X_USB"
        // we have to pre-set index 0, so that the real indices start with 1
        var featureNamesTmp: List[String] = List("--dummy--")
        val featureMap: scala.collection.mutable.HashMap[String, SingleFeatureExpr] = new scala.collection.mutable.HashMap()
        var currentLine: Int = 1

        for (line: String <- Source.fromFile(dimacsFile).getLines().takeWhile(_.startsWith("c"))) {
            val lineElements: Array[String] = line.split(" ")
            if (!lineElements(1).endsWith("$")) {
                // feature indices ending with $ are artificial and can be ignored here
                assert(augmentString(lineElements(1)).toInt.equals(currentLine), "\"" + lineElements(1) + "\"" + " != " + currentLine)
                featureNamesTmp ::= lineElements(2)
            }
            currentLine += 1
        }

        // maintain a hashmap that maps feature names to corresponding feature expressions (SingleFeatureExpr)
        // we only store those features that occur in the file (keeps configuration small);
        // the rest is not important for the configuration;
        val featureNames: Array[String] = featureNamesTmp.reverse.toArray
        featureNamesTmp = null
        for (i <- 0.to(featureNames.length - 1)) {
            val searchResult = features.find(_.feature.equals(fnamePrefix + featureNames(i)))
            if (searchResult.isDefined) {
                featureMap.update(featureNames(i), searchResult.get)
            }
        }

        // parse configurations
        // format is:
        // Feature\Product;0;..;N;       // number of Products (N+1)
        // FeatureA;-;X;....;            // exclusion of FeatureA in Product 0 and inclusion of FeatureA in Product 1
        // FeatureB                      // additional features
        // ...
        val csvLines = Source.fromFile(csvFile).getLines()
        val numProducts = csvLines.next().split(";").last.toInt + 1

        // create and initialize product configurations array
        val pconfigurations = new Array[(List[SingleFeatureExpr], List[SingleFeatureExpr])](numProducts)
        for (i <- 0 to numProducts - 1) {
            pconfigurations.update(i, (List(), List()))
        }

        // iterate over all lines with Features, determine the selection/deselection in available products and add it to
        // product configurations (true features / false features)
        while (csvLines.hasNext) {
            val featureLine = csvLines.next().split(";")

            for (i <- 1 to numProducts) {
                if (featureMap.contains(featureLine(0))) {
                    var product = pconfigurations(i - 1)
                    if (featureLine(i) == "X") {
                        product = product.copy(_1 = featureMap(featureLine(0)) :: product._1)
                    } else {
                        product = product.copy(_2 = featureMap(featureLine(0)) :: product._2)
                    }
                    pconfigurations.update(i - 1, product)
                }
            }
        }

        // create a single configuration from the true features and false features list
        for (i <- 0 to pconfigurations.length - 1) {
            val config = new SimpleConfiguration(pconfigurations(i)._1, pconfigurations(i)._2)

            // need to check the configuration here again.
            if (!config.toFeatureExpr.getSatisfiableAssignment(fm, features.toSet, 1 == 1).isDefined) {
                println("no satisfiable solution for product (" + i + "): " + csvFile)
            } else {
                retList ::= config
            }
        }

        (retList, "Generated Configs: " + retList.size + "\n")
    }

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
        val path = {
            if (filePath.startsWith("file")) filePath.substring(5)
            else filePath
        }
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

    def getFileName(originalFilePath: String) = originalFilePath.substring(originalFilePath.lastIndexOf(File.separatorChar), originalFilePath.length).replace("/", "")

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
                case x => List(x).:::(readIn(reader))
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
        (trueFeatures.toList, assignValues)
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

    def isSystemLinkedName(name : String) = SystemLinker.allLibs.par.contains(name)
}