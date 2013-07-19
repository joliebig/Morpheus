package de.fosd.typechef.crefactor.BusyBoxEvaluation

import de.fosd.typechef.crefactor.util.EvalHelper
import java.io.File
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel, FeatureExpr, SingleFeatureExpr}
import de.fosd.typechef.parser.c.AST

trait BBEval extends EvalHelper {

    val caseStudyPath = "../busybox/"
    val completeBusyBoxPath = new File(caseStudyPath).getCanonicalPath
    val busyBoxFiles: String = completeBusyBoxPath + "/busybox_files"
    val busyBoxPath = completeBusyBoxPath + "/busybox-1.18.5/"
    val busyBoxPathUntouched = completeBusyBoxPath + caseStudyPath + "/busybox-1.18.5_untouched/"
    val result = "/result/"

    val filterFeatures = List("def(CONFIG_SELINUX)", "CONFIG_SELINUX", "def(CONFIG_TCPSVD)", "CONFIG_TCPSVD", "def(CONFIG_UDPSVD)", "CONFIG_UDPSVD", "def(CONFIG_MKFS_EXT2)", "CONFIG_MKFS_EXT2")
    val allFeaturesFile = getClass.getResource("/BusyBoxAllFeatures.config").getFile
    val allFeatures = getAllFeaturesFromConfigFile(null, new File(allFeaturesFile))
    val pairWiseFeaturesFile = getClass.getResource("/busyBox_pairwise.configs").getFile

    val systemProperties: String = completeBusyBoxPath + "/redhat.properties"
    val includeHeader: String = completeBusyBoxPath + "/config.h"
    val includeDir: String = completeBusyBoxPath + "/busybox-1.18.5/include"
    val featureModel: String = completeBusyBoxPath + "/featureModel"
    val featureModel_DIMACS: String = completeBusyBoxPath + "/BB_fm.dimacs"

}

object PrepareRefactoredASTforEval extends BBEval {

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


