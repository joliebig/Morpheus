package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5

import java.io._
import de.fosd.typechef.crefactor.evaluation.Evaluation
import scala.io.Source


trait BusyBoxEvaluation extends Evaluation {

    val evalName = "busybox-1.18.5"
    val caseStudyPath = "../cRefactor-BusyBoxEvaluation/"
    val completePath = new File(caseStudyPath).getCanonicalPath
    val filesToEval: String = completePath + "/busybox_files"
    val evalFiles = getEvaluationFiles
    val blackListFiles: List[String] = Source.fromFile(getClass.getResource("/busybox_blacklist").getFile).getLines().toList
    val blackListIds: List[String] = List()
    val sourcePath = completePath + "/" + evalName + "/"
    val testPath = completePath + "/" + evalName + "/"
    val result = "/result/"

    val filterFeatures = List("def(CONFIG_SELINUX)", "CONFIG_SELINUX", "def(CONFIG_TCPSVD)", "CONFIG_TCPSVD", "def(CONFIG_UDPSVD)", "CONFIG_UDPSVD", "def(CONFIG_MKFS_EXT2)", "CONFIG_MKFS_EXT2")
    val allFeaturesFile = getClass.getResource("/BusyBoxAllFeatures.config").getFile
    val allFeatures = getAllFeaturesFromConfigFile(null, new File(allFeaturesFile))
    val pairWiseFeaturesFile = getClass.getResource("/busyBox_pairwise.configs").getFile
    val existingConfigsDir: String = completePath + "/existing_configs/"

    val featureModel: String = completePath + "/featureModel"
    val featureModel_DIMACS: String = completePath + "/BB_fm.dimacs"
    val featuresFile: String = completePath + "/features"

    val runTimeout = 300000

    val FORCE_VARIABILITY = true
    val FORCE_LINKING = false

}



