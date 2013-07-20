package de.fosd.typechef.crefactor.BusyBoxEvaluation

import java.io.{FilenameFilter, File}
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr, SingleFeatureExpr, FeatureModel}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.util.Verification

object BusyBoxVerification extends BusyBoxEvaluation with Verification {

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean = {
        val workingPath = bbFile.getCanonicalPath
        val orgFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched"))
        val refFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + bbFile.getName)
        val resultDir = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/")

        val configs = resultDir.listFiles(new FilenameFilter {
            def accept(input: File, file: String): Boolean = file.endsWith(".config")
        })

        val result = configs.map(config => {
            def buildAndTest(busyBoxFile: File, ext: String): (Boolean, String) = {
                val buildResult = build
                val testResult = runTest
                writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + config.getName + ext + ".build")
                if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + config.getName + ext + ".buildErr")
                writeResult(testResult, resultDir.getCanonicalPath + "/" + config.getName + ext + ".test")
                busyBoxFile.delete()
                (buildResult._1, testResult)
            }

            // clean dir first
            runScript("./buildClean.sh", busyBoxPath)

            val configBuild = new File(busyBoxPath + ".config")
            copyFile(config, configBuild)

            val orgTest = buildAndTest(bbFile, "_org")

            val buildRefFile = new File(workingPath)

            // clean dir first
            runScript("./buildClean.sh", busyBoxPath)

            // Replace original file with refactored file
            copyFile(refFile, buildRefFile)

            val refTest = buildAndTest(buildRefFile, "_ref")

            // Restore old original file again
            copyFile(orgFile, new File(workingPath))
            configBuild.delete()

            if (!orgTest._1) {
                writeError("Invalid Config.\n", resultDir.getCanonicalPath + "/" + config.getName, run)
                writeResult("Invalid Config", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                true
            } else if (refTest._1) {
                writeResult(orgTest.equals(refTest).toString, resultDir.getCanonicalPath + "/" + config.getName + ".result")
                val succ = orgTest.equals(refTest)
                if (!succ) writeError("Test failed!\n", resultDir.getCanonicalPath + "/" + config.getName, run)
                succ
            } else {
                writeError("Refactor build failed!\n", resultDir.getCanonicalPath + "/" + config.getName, run)
                writeResult("Refactor build failed!", resultDir.getCanonicalPath + "/" + config.getName + ".result")
                false
            }

        })
        !result.contains(false)
    }

    def runTest: String = {
        val result = runScript("./runtest", busyBoxPath + "testsuite/")
        val stream = streamsToString(result)
        stream._1
    }

    def build: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", busyBoxPath)
        val stream = streamsToString(result)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

}

object PrepareASTforVerification extends BusyBoxEvaluation {

    private def genAllConfigVariantsForFeatures(enabledFeatures: List[SingleFeatureExpr], affectedFeatures: List[FeatureExpr], fm: FeatureModel, dir: File): List[List[SingleFeatureExpr]] = {
        var wrongCounter = 0
        val singleAffectedFeatures = affectedFeatures.flatMap(_.collectDistinctFeatureObjects.filterNot(ft => filterFeatures.contains(ft.feature))).distinct
        // default start config, it all starts from this config
        val startConfig = List(enabledFeatures)

        // iterate over every affected feature and activate or deactivate it on all configs and generated configes
        singleAffectedFeatures.foldLeft(startConfig)((configs, singleAffectFeature) => {
            configs ::: configs.map(config => {
                var generatedConfig: List[SingleFeatureExpr] = List()
                if (config.contains(singleAffectFeature)) generatedConfig = config.diff(List(singleAffectFeature))
                else generatedConfig = singleAffectFeature :: config

                val generatedFeatureExpr = generatedConfig.foldLeft(FeatureExprFactory.True)((fExpr, singleFxpr) => {
                    fExpr.and(singleFxpr)
                })

                if (generatedFeatureExpr.isSatisfiable(fm)) generatedConfig
                else {
                    writeConfig(generatedConfig, dir, wrongCounter + ".invalidConfig")
                    wrongCounter += 1
                    List()
                }
            }).distinct
        })
    }

    def makeConfigs(refactored: AST, fm: FeatureModel, originalFilePath: String, affectedFeatures: List[FeatureExpr], run: Int) {
        val dir = getResultDir(originalFilePath, run)

        val configRes = getClass.getResource("/busybox_Configs/")
        val configs = new File(configRes.getFile)

        initializeFeatureList(refactored)
        val pairWiseConfigs = loadConfigurationsFromCSVFile(new File(pairWiseFeaturesFile), new File(featureModel_DIMACS), features, fm, "CONFIG_")

        var pairCounter = 0

        pairWiseConfigs._1.foreach(pairConfig => {
            val enabledFeatures = pairConfig.getTrueSet.filterNot(ft => filterFeatures.contains(ft.feature))
            writeConfig(enabledFeatures, dir, pairCounter + "pairwise.config")
            pairCounter += 1
        })


        val generatedConfigs = configs.listFiles().map(config => {
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
        })
    }

}


