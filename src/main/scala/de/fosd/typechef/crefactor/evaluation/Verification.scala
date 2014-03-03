package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.{FeatureExprFactory, SingleFeatureExpr, FeatureExpr, FeatureModel}
import java.io.File
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.{FileFeatures, ConfigurationHandling}
import de.fosd.typechef

trait Verification extends Evaluation {

    val buildScript: String = "./build.sh"
    val testScript: String = "./runtest.sh"
    val cleanScript: String = "./clean.sh"

    def verify(evalFile: String, fm: FeatureModel, mode: String) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/" + mode + "/")
        if (!resultDir.exists) resultDir.mkdirs

        val buildResult = build
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + "build")
        if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + "buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + "test")
        if (!testResult._1) writeResult(testResult._3, resultDir.getCanonicalPath + "/" + "testError")

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + "result")
    }

    def test: (Boolean, String, String) = {
        val result = runScript(testScript, testPath)
        evaluateScriptResult(result)
    }

    def build: (Boolean, String, String) = {
        val result = runScript(buildScript, sourcePath)
        evaluateScriptResult(result)
    }

    def writeConfig(config: typechef.SimpleConfiguration, dir: File, name: String): Unit = writeConfig(config.getTrueSet, dir, name)

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

    def generateEvaluationConfigurations(tunit: TranslationUnit, fm: FeatureModel, originalFilePath: String,
                                         affectedFeatures: List[List[FeatureExpr]]) {
        val resultDir = getResultDir(originalFilePath)

        val existingConfigs = new File(existingConfigsDir)
        //val pwConfig = ConfigurationHandling.buildConfigurationsPairwise(tunit, ff, fm, null, null, "busybox", List())

        // TOOD Ask Jörg move to config handling
        initializeFeatureList(tunit)
        val pairWiseConfigs =
            loadConfigurationsFromCSVFile(new File(pairWiseFeaturesFile), new File(featureModel_DIMACS), features, fm, "CONFIG_")

        var pairCounter = 0

        pairWiseConfigs._1.foreach(pairConfig => {
            val enabledFeatures = pairConfig.getTrueSet.filterNot(ft => filterFeatures.contains(ft.feature))
            writeConfig(enabledFeatures, resultDir, pairCounter + "pairwise.config")
            pairCounter += 1
        })

        val generatedConfigs = variabilityCoverage(existingConfigs, fm, affectedFeatures)

        if (generatedConfigs.size > maxConfigs) {
            val ff = new FileFeatures(tunit)
            val codeCoverage =
                ConfigurationHandling.configurationCoverage(tunit, fm, ff, List(), preferDisabledFeatures = false)
            codeCoverage._1.foldLeft(0)((counter, coverageConf) => {
                writeConfig(coverageConf, resultDir, counter + "coverage.config")
                counter + 1
            }

            )
        } else {
            generatedConfigs.foreach(genConfigs => {
                var configNumber = 0
                val name = genConfigs._1.getName
                genConfigs._2.foreach(genConfig => {
                    writeConfig(genConfig, resultDir, configNumber + name)
                    configNumber += 1
                })
            })
        }
    }

    def variabilityCoverage(existingConfigs: File, fm: FeatureModel, affectedFeatures: List[List[FeatureExpr]],
                            startCounter: Int = 0) = {
        var genCounter = startCounter
        logger.info("Loading following configs: " + existingConfigs.listFiles())
        existingConfigs.listFiles().flatMap(config => {
            if (genCounter > maxConfigs) None
            else {
                val enabledFeatures = getEnabledFeaturesFromConfigFile(fm, config)
                val genConfigs =
                    affectedFeatures.foldLeft(List[List[SingleFeatureExpr]]())((genConfigs, singleAffectedFeatures) => {
                        if (genCounter > maxConfigs) genConfigs
                        else {
                            val generated =
                                genAllConfigVariantsForFeatures(
                                    enabledFeatures, singleAffectedFeatures, fm, genCounter).distinct
                            genCounter += generated.size
                            genConfigs ::: generated
                        }
                    })
                Some(config, genConfigs)
            }
        })
    }

    private def genAllConfigVariantsForFeatures(enabledFeatures: List[SingleFeatureExpr],
                                                affectedFeatures: List[FeatureExpr], fm: FeatureModel,
                                                startCounter: Int = 0): List[List[SingleFeatureExpr]] = {

        val singleAffectedFeatures =
            affectedFeatures.flatMap(_.collectDistinctFeatureObjects.filterNot(
                ft => filterFeatures.contains(ft.feature))).distinct

        logger.info("Single AffectedFeatures: " + singleAffectedFeatures.size)

        // default start config, it all starts from this config
        val startConfig = List(enabledFeatures)
        var genCounter = startCounter
        var wrongCounter = 0

        // iterate over every affected feature and activate or deactivate it on all configs and generated configes
        singleAffectedFeatures.foldLeft(startConfig)((configs, singleAffectFeature) => {
            if (genCounter > maxConfigs) return configs
            logger.info("Generating configs for single affected feature: " + singleAffectFeature)
            configs ::: configs.flatMap(config => {
                if (genCounter > maxConfigs) None
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
                        Some(generatedConfig.sortBy(_.feature))
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
}
