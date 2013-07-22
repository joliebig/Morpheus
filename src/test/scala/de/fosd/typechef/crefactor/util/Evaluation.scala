package de.fosd.typechef.crefactor.util

import java.io._
import de.fosd.typechef.parser.c.{PrettyPrinter, AST}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr, SingleFeatureExpr, FeatureModel}
import java.util.regex.Pattern
import scala.io.Source
import de.fosd.typechef.crefactor.{MorphFrontend, Logging}
import java.util.IdentityHashMap
import java.util
import de.fosd.typechef.parser.c.GnuAsmExpr
import de.fosd.typechef.parser.c.Id
import scala.collection.immutable.HashMap
import de.fosd.typechef.conditional.{Opt, Choice}

trait Evaluation extends Logging {

    val caseStudyPath: String
    val completeBusyBoxPath: String
    val busyBoxFiles: String
    val busyBoxPath: String
    val busyBoxPathUntouched: String
    val result: String

    val filterFeatures: List[String]
    val allFeaturesFile: String
    val allFeatures: (List[SingleFeatureExpr], IdentityHashMap[String, String])
    val pairWiseFeaturesFile: String

    val systemProperties: String
    val includeHeader: String
    val includeDir: String
    val featureModel: String
    val featureModel_DIMACS: String


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

    def runScript(script: String, dir: String): (InputStream, InputStream) = {
        val pb = new ProcessBuilder(script)
        pb.directory(new File(dir))
        val p = pb.start()
        p.waitFor()
        (p.getInputStream, p.getErrorStream)
    }


    def writeResult(result: String, file: String) = {
        var out: FileWriter = null
        if (file.startsWith(".")) out = new java.io.FileWriter(file.replaceFirst(".", ""))
        else out = new java.io.FileWriter(file)

        out.write(result)
        out.flush()
        out.close()
    }

    def writeAST(ast: AST, filePath: String) {
        val writer = new FileWriter(filePath)
        val prettyPrinted = PrettyPrinter.print(ast)
        writer.write(prettyPrinted.replaceAll("definedEx", "defined"))
        writer.flush()
        writer.close()
    }

    def writeError(error: String, originalFilePath: String, run: Int) = {
        val out = new java.io.FileWriter(originalFilePath + ".error")
        out.write(error)
        out.write("\n")
        out.flush()
        out.close()
    }

    def writeExeception(exception: String, originalFilePath: String, run: Int) = {
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

    def writeConfig(config: Set[SingleFeatureExpr], dir: File, name: String): Unit = writeConfig(config.toList, dir, name)

    def writeConfig(config: List[SingleFeatureExpr], dir: File, name: String) {
        val out = new java.io.FileWriter(dir.getCanonicalPath + File.separatorChar + name)
        val disabledFeatures = allFeatures._1.diff(config)
        config.foreach(feature => {
            val ft = feature.feature
            out.write(ft + "=y")
            out.write("\n")
        })
        disabledFeatures.foreach(feature => {
            val ft = feature.feature
            if (allFeatures._2.containsKey(feature.feature)) out.write(ft + "=" + allFeatures._2.get(feature.feature))
            else out.write("# " + ft + " is not set")
            out.write("\n")
        })
        out.flush()
        out.close()
    }

    def getFileName(originalFilePath: String) = originalFilePath.substring(originalFilePath.lastIndexOf(File.separatorChar), originalFilePath.length)

    def getResultDir(originalFilePath: String, run: Int): File = {
        val outputFilePath = originalFilePath.replace("busybox-1.18.5", "result")
        val result = new File(outputFilePath + File.separatorChar + run + File.separatorChar)
        if (!result.exists()) result.mkdirs()
        result
    }

    def parse(file: File): (AST, FeatureModel) = MorphFrontend.parse(file.getAbsolutePath, systemProperties, includeHeader, includeDir, featureModel)

    def getAllRelevantIds(a: Any): List[Id] = {
        a match {
            case id: Id => if (!(id.name.startsWith("__builtin"))) List(id) else List()
            case gae: GnuAsmExpr => List()
            case l: List[_] => l.flatMap(x => getAllRelevantIds(x))
            case p: Product => p.productIterator.toList.flatMap(x => getAllRelevantIds(x))
            case k => List()
        }
    }

    def analsyeDeclUse(map: IdentityHashMap[Id, List[Id]]): List[Int] = map.keySet().toArray(Array[Id]()).map(key => map.get(key).length).toList

    def getBusyBoxFiles: List[String] = {
        def readIn(reader: BufferedReader): List[String] = {
            reader.readLine() match {
                case null => List()
                case x => List(x + ".c").:::(readIn(reader))
            }
        }
        val reader = new BufferedReader(new FileReader(busyBoxFiles))
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
        val assignValues = new util.IdentityHashMap[String, String]()

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
}