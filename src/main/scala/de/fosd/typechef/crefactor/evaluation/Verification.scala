package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExprFactory, SingleFeatureExpr, FeatureExpr, FeatureModel}
import java.io.{Writer, File}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef._
import scala.Some
import de.fosd.typechef.parser.c.TranslationUnit

trait Verification extends Evaluation {

    val buildScript: String = "./build.sh"
    val testScript: String = "./runtest.sh"
    val confScript: String = "./genConfig.sh"
    val cleanScript: String = "./clean.sh"
    val configFlags: String = "configFlags"

    def configBasedVerification(evalFile: String, configs : List[(String, List[SingleFeatureExpr])]) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/")
        if (!resultDir.exists)
            resultDir.mkdirs

        val configPaths = new StringBuilder

        configs.foreach(config => {
            writeKConfig(config._2, resultDir, config._1)
            configPaths.append(resultDir.getCanonicalPath + File.separator + config._1 + "\n")
        })

        val fw = new java.io.FileWriter(new File(completePath + "/" + evalName + "/" + configFlags))

        fw.write(configPaths.toString)
        fw.flush
        fw.close
    }

    def featureBasedVerification(evalFile: String, fm: FeatureModel, affectedFeatures: List[FeatureExpr] = List()) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/")
        if (!resultDir.exists)
            resultDir.mkdirs

        val confFeatures = new ConfigFeatures(allFeatures._1)

        // get features
        val featureCombinations = getFeatureCombinations(confFeatures, affectedFeatures)

        val fw = new java.io.FileWriter(new File(completePath + "/" + evalName + "/" + configFlags))
        // addDefconfig
        fw.write(" \n")

        val noFiltering =
            if (evalName.equals("sqlite")) true
            else false

        // addAllOtherConfigs
        var writtenConfigs = 0
        featureCombinations.foreach(config => {
            if (writeConfigFlags(config, fw, noFiltering)) writtenConfigs += 1
        })

        StatsCan.addStat(evalFile, Stats.Variants, writtenConfigs)

        fw.flush
        fw.close
    }

    def test: (Boolean, String, String) = {
        val result = runScript(testScript, testPath)
        evaluateScriptResult(result)
    }

    def build: (Boolean, String, String) = {
        val result = runScript(buildScript, sourcePath)
        evaluateScriptResult(result)
    }

    def buildAndTest(resultDir: File, run: Int, mode: String): Boolean = {
        val result = runScript(buildScript, sourcePath, runTimeout)
        val buildResult = evaluateScriptResult(result)
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + run + mode + ".build")
        if (!buildResult._1)
            writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + run + mode + ".buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + run + mode + ".test")
        if (!testResult._1)
            writeResult(testResult._3, resultDir.getCanonicalPath + "/" + run + mode + ".testError")

        logger.info("Pass build: " + buildResult._1)
        logger.info("Pass test: " + testResult._1)

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + run + mode + ".result")
        testResult._1 && buildResult._1
    }

    def writeConfigFlags(configuration : SimpleConfiguration, writer : Writer, noFiltering : Boolean = false) : Boolean = {
       val features = configuration.getTrueSet.flatMap(x => {
           if (noFiltering || filterFeatures.contains(x.feature)) Some(x.feature)
           else None
        }).mkString("-D", " -D", "")

       if (features.nonEmpty) {
           writer.write(features)
           writer.write("\n")
       }

       features.nonEmpty
    }

    def writeKConfig(config: List[SingleFeatureExpr], dir: File, name: String) {
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

    def generateEvaluationConfigurations(tunit: TranslationUnit, fm: FeatureModel, originalFilePath: String,
                                         affectedFeatures: List[List[FeatureExpr]]): List[(String, List[SingleFeatureExpr])] = {
        val existingConfigs = new File(existingConfigsDir)
        val tUnitFeatures = new TUnitFeatures(tunit)

        val generatedConfigs = variabilityCoverage(existingConfigs, fm, affectedFeatures)

        if (generatedConfigs.size > maxConfigs - 1) {
            val codeCoverage =
                ConfigurationHandling.codeCoverage(tunit, fm, tUnitFeatures, List(), preferDisabledFeatures = false)

            val covConfigs = codeCoverage._1.zipWithIndex.map(coverageConf => 
                (coverageConf._2 + "coverage.config", coverageConf._1.getTrueSet.toList))

            val pairWiseConfigs =
                ConfigurationHandling.loadConfigurationsFromCSVFile(new File(pairWiseFeaturesFile),
                    new File(featureModel_DIMACS), tUnitFeatures, fm, "CONFIG_")

            val parConfigs = pairWiseConfigs._1.zipWithIndex.map(pairConfig => {
                val enabledFeatures = pairConfig._1.getTrueSet.filterNot(ft => filterFeatures.contains(ft.feature))
                val confName = pairConfig._2 + "pairwise.config"
                (confName, enabledFeatures.toList)
            })

            covConfigs ::: parConfigs
        } else 
            generatedConfigs.flatMap(genConfigs => {
                val name = genConfigs._1.getName
                genConfigs._2.zipWithIndex.map(genConfig => (genConfig._2 + name, genConfig._1.getTrueSet.toList))
            }).toList
    }

    def variabilityCoverage(existingConfigs: File, fm: FeatureModel, affectedFeatures: List[List[FeatureExpr]],
                            startCounter: Int = 0)  = {
        val generatedSimpleConfigurations = new scala.collection.mutable.HashSet[SimpleConfiguration]
        var genCounter = startCounter
        logger.info("Loading from " + existingConfigs.getCanonicalPath + " following configs: " + existingConfigs.listFiles())
        existingConfigs.listFiles().flatMap(config => {
            if (genCounter > maxConfigs) None
            else {
                val enabledFeatures = getEnabledFeaturesFromConfigFile(fm, config)
                val confFeatures = new ConfigFeatures(allFeatures._1)
                val singleAffectedFeatures = affectedFeatures.flatten.distinct
                val generatedConfigs =
                    genAllConfigVariantsForFeatures(confFeatures, enabledFeatures, singleAffectedFeatures, fm, genCounter)
                val filteredGenerated =
                    generatedConfigs.flatMap(genConfig => {
                        if (!generatedSimpleConfigurations.contains(genConfig)) {
                            generatedSimpleConfigurations.add(genConfig)
                            Some(genConfig)
                        }
                        else None
                    })
                genCounter += filteredGenerated.size
                Some(config, filteredGenerated)
            }
        })
    }

    private def genAllConfigVariantsForFeatures(ff: KnownFeatures, enabledFeatures: List[SingleFeatureExpr],
                                                affectedFeatures: List[FeatureExpr], fm: FeatureModel,
                                                startCounter: Int = 0): List[SimpleConfiguration] = {

        val singleAffectedFeatures =
            affectedFeatures.flatMap(_.collectDistinctFeatureObjects.filterNot(
                ft => filterFeatures.contains(ft.feature))).distinct

        logger.info("Single AffectedFeatures: " + singleAffectedFeatures.size)

        // default start config, it all starts from this config

        val startConfig = new SimpleConfiguration(ff, enabledFeatures, List())
        var genCounter = startCounter
        var wrongCounter = 0

        // iterate over every affected feature and activate or deactivate it on all configs and generated configes
        singleAffectedFeatures.foldLeft(List(startConfig))((configs, singleAffectFeature) => {
            if (genCounter > maxConfigs) return configs
            logger.info("Generating configs for single affected feature: " + singleAffectFeature)
            configs ::: configs.flatMap(config => {
                if (genCounter > maxConfigs) None
                else {
                    val genTime = new StopClock
                    val genTrueSet: List[SingleFeatureExpr] =
                        if (config.trueSet.contains(singleAffectFeature)) config.trueSet.diff(List(singleAffectFeature))
                        else singleAffectFeature :: config.trueSet

                    val generatedFeatureExpr = genTrueSet.foldLeft(FeatureExprFactory.True)((fExpr, singleFxpr) => {
                        fExpr.and(singleFxpr)
                    })

                    if (generatedFeatureExpr.isSatisfiable(fm)) {
                        genCounter += 1
                        logger.info("Generated config number: " + genCounter + " in " + genTime.getTime + "ms.")
                        Some(new SimpleConfiguration(ff, genTrueSet, List()))
                    }
                    else {
                        wrongCounter += 1
                        logger.info("Generated invalid config number: " + wrongCounter + " in " + genTime.getTime + "ms.")
                        None
                    }
                }
            }).distinct
        })
    }

    def getFeatureCombinations(ff: KnownFeatures, affectedFeatures: List[FeatureExpr]): List[SimpleConfiguration] = {
        affectedFeatures.flatMap(affectedFeature => {
            affectedFeature.collectDistinctFeatureObjects.foldRight(List[SimpleConfiguration]())((feature, genConfigs) => {
                if (genConfigs.isEmpty) List(new SimpleConfiguration(ff, List(feature), List()))
                else {
                    genConfigs ::: genConfigs.flatMap(config => {
                        val genTrueSet: List[SingleFeatureExpr] =
                            if (config.trueSet.contains(feature)) config.trueSet.diff(List(feature))
                            else feature :: config.trueSet
                        Some(new SimpleConfiguration(ff, genTrueSet, List()))
                    })
                }
            })
        }).distinct
    }
}
