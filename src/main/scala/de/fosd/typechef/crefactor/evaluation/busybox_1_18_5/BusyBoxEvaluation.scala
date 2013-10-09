package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5

import java.io._
import de.fosd.typechef.parser.c.{ConditionalNavigation, ASTNavigation}
import de.fosd.typechef.crefactor.evaluation.Evaluation


trait BusyBoxEvaluation extends Evaluation with ASTNavigation with ConditionalNavigation {

    val caseStudyPath = "../busybox/"
    val completeBusyBoxPath = new File(caseStudyPath).getCanonicalPath
    val busyBoxFiles: String = completeBusyBoxPath + "/busybox_files"
    val busyBoxPath = completeBusyBoxPath + "/busybox-1.18.5/"
    val busyBoxPathUntouched = completeBusyBoxPath + "/busybox-1.18.5_untouched/"
    val result = "/result/"
    val featureModelPath = completeBusyBoxPath + "/featureModel"

    val filterFeatures = List("def(CONFIG_SELINUX)", "CONFIG_SELINUX", "def(CONFIG_TCPSVD)", "CONFIG_TCPSVD", "def(CONFIG_UDPSVD)", "CONFIG_UDPSVD", "def(CONFIG_MKFS_EXT2)", "CONFIG_MKFS_EXT2")
    val allFeaturesFile = getClass.getResource("/BusyBoxAllFeatures.config").getFile
    val allFeatures = getAllFeaturesFromConfigFile(null, new File(allFeaturesFile))
    val pairWiseFeaturesFile = getClass.getResource("/busyBox_pairwise.configs").getFile

    val systemProperties: String = completeBusyBoxPath + "/redhat.properties"
    val includeHeader: String = completeBusyBoxPath + "/config.h"
    val includeDir: String = completeBusyBoxPath + "/busybox-1.18.5/include"
    val featureModel: String = completeBusyBoxPath + "/featureModel"
    val featureModel_DIMACS: String = completeBusyBoxPath + "/BB_fm.dimacs"
    val featuresFile: String = completeBusyBoxPath + "/features"

    val FORCE_VARIABILITY = true
    val MAX_DEPTH = 27
    val amountOfRefactorings = 3
    val MAX = 1
}



