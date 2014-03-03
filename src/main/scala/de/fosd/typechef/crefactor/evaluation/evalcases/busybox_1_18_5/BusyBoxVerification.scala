package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5

import java.io.{FilenameFilter, File}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr, SingleFeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.evaluation.{StatsCan, Verification}
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.ConfigurationHandling

object BusyBoxVerification extends BusyBoxEvaluation with Verification {


    override def verify(evalFile: String, fm: FeatureModel, mode: String): Unit = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result"))
        val configs = resultDir.listFiles(new FilenameFilter {
            def accept(input: File, file: String): Boolean = file.endsWith(".config")
        })

        StatsCan.addStat(evalFile, Variants, configs.length)

        val result = configs.map(config => {
            logger.info("Testing config: " + config.getName + " for " + evalFile + " in mode: " + mode + ".")
            def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
                val buildTestTime = System.currentTimeMillis()
                val buildResult = build
                val testResult = runTest
                logger.info("Build and test duration in ms: " + (System.currentTimeMillis() - buildTestTime))
                writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + config.getName + ext + ".build")
                if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + config.getName + ext + ".buildErr")
                writeResult(testResult, resultDir.getCanonicalPath + "/" + config.getName + ext + ".test")
                (buildResult._1, testResult)
            }

            val configBuild = new File(sourcePath + ".config")

            copyFile(config, configBuild)

            val buildTest = buildAndTest(new File(evalFile), mode)

            configBuild.delete()

            if (!buildTest._1) {
                writeError("Invalid Config.\n", resultDir.getCanonicalPath + "/" + config.getName)
                writeResult("Invalid Config", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                logger.error("Invalid config: " + config.getName + " for " + evalFile + " in mode: " + mode)
                true
            } else if (buildTest._1) {
                writeResult("true", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                logger.info("Test passed: " + config.getName + " for " + evalFile + " in mode: " + mode)
                true
            } else {
                writeError("Refactor build failed!\n", resultDir.getCanonicalPath + "/" + config.getName)
                writeResult("Refactor build failed!", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                logger.info("Test failed: " + config.getName + " for " + evalFile + " in mode: " + mode)
                false
            }
        })
        logger.info(evalFile + " passed build and testing as " + mode + ": " + result.exists(x => x))
    }

    def runTest: String = {
        val result = runScript("./runtest", sourcePath + "testsuite/")
        val stream = streamsToString(result)
        runScript("./cleanTests.sh", sourcePath)
        stream._1
    }

    override def build: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", sourcePath)
        evaluateScriptResult(result)
    }

}

object PrepareTUnitForVerification extends BusyBoxEvaluation {

    private def genAllConfigVariantsForFeatures(enabledFeatures: List[SingleFeatureExpr], affectedFeatures: List[FeatureExpr], fm: FeatureModel, dir: File): List[List[SingleFeatureExpr]] = {
        var wrongCounter = 0
        val singleAffectedFeatures = affectedFeatures.flatMap(_.collectDistinctFeatureObjects.filterNot(ft => filterFeatures.contains(ft.feature))).distinct
        logger.info("Single AffectedFeatures: " + singleAffectedFeatures.size)

        // default start config, it all starts from this config
        val startConfig = List(enabledFeatures)

        // iterate over every affected feature and activate or deactivate it on all configs and generated configes
        var genCounter = 0
        singleAffectedFeatures.foldLeft(startConfig)((configs, singleAffectFeature) => {
            // TODO current debug break for big configs in extract - will be replaced soon by a new approach - so just looking away from the following code
            if (genCounter > 5) return configs
            logger.info("Generating configs for single affected feature: " + singleAffectFeature)
            configs ::: configs.flatMap(config => {
                if (genCounter > 5) None
                else {
                    val genTime = new StopClock
                    var generatedConfig: List[SingleFeatureExpr] = List()
                    if (config.contains(singleAffectFeature)) generatedConfig = config.diff(List(singleAffectFeature))
                    else generatedConfig = singleAffectFeature :: config

                    val generatedFeatureExpr = generatedConfig.foldLeft(FeatureExprFactory.True)((fExpr, singleFxpr) => {
                        fExpr.and(singleFxpr)
                    })

                    if (generatedFeatureExpr.isSatisfiable(fm)) {
                        genCounter += 1
                        logger.info("Generated config number: " + genCounter + " in " + genTime.getTime + "ms.")
                        Some(generatedConfig)
                    }
                    else {
                        wrongCounter += 1
                        logger.info("Generated invalid config number: " + wrongCounter + " in " + genTime.getTime + "ms.")
                        writeConfig(generatedConfig, dir, wrongCounter + ".invalidConfig")
                        None
                    }
                }
            }).distinct
        })
    }

    def makeConfigs(refactored: AST, fm: FeatureModel, originalFilePath: String, affectedFeatures: List[List[FeatureExpr]]) {
        val dir = getResultDir(originalFilePath)
        val configsDir = new File(getClass.getResource("/busybox_Configs/").getFile)

        initializeFeatureList(refactored)
        val pairWiseConfigs = loadConfigurationsFromCSVFile(new File(pairWiseFeaturesFile), new File(featureModel_DIMACS), features, fm, "CONFIG_")

        var pairCounter = 0

        pairWiseConfigs._1.foreach(pairConfig => {
            val enabledFeatures = pairConfig.getTrueSet.filterNot(ft => filterFeatures.contains(ft.feature))
            writeConfig(enabledFeatures, dir, pairCounter + "pairwise.config")
            pairCounter += 1
        })

        /**
        val generatedConfigs = configsDir.listFiles().map(config => {
            val enabledFeatures = getEnabledFeaturesFromConfigFile(fm, config)
            (config, genAllConfigVariantsForFeatures(enabledFeatures, affectedFeatures, fm, dir))
        })

        generatedConfigs.foreach(genConfigs => {
            var configNumber = 0
            val name = genConfigs._1.getName
            genConfigs._2.foreach(genConfig => {
                writeConfig(genConfig, dir, configNumber + name)
                configNumber += 1
            })
        }) */
    }

}


