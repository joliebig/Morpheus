package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5

import java.io._
import de.fosd.typechef.crefactor.evaluation.{StatsCan, Refactoring, Evaluation}
import scala.io.Source
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.refactor.{Inline, Extract, Rename}
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crefactor.backend.CModuleInterface
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.crefactor.evaluation.util.StopClock


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

    val renameEngine = Rename
    val extractEngine = Extract
    val inlineEngine = Inline

    override def evaluate(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CModuleInterface, r: Refactoring): Unit = {
        logger.info("File to engine: " + getFileName(file) + " +++")
        val resultDir = getResultDir(file)
        val path = resultDir.getCanonicalPath + File.separatorChar + getFileName(file)
        if (tunit == null) logger.error("+++ AST is null! +++")
        else if (blackListFiles.exists(getFileName(file).equalsIgnoreCase)) println("+++ File is blacklisted and cannot be build +++")
        else {
            try {
                val morpheus = new Morpheus(tunit, fm, linkInterface, file)
                // reset test environment
                runScript("./cleanAndReset.sh", sourcePath)
                val result = r.refactor(morpheus)
                if (result._1) {
                    write(result._2, morpheus.getFile)
                    result._4.foreach(linked => writePrettyPrintedTUnit(linked._2, linked._1))
                    logger.info("Features: " + result._3)
                    BusyBoxVerification.generateEvaluationConfigurations(result._2, morpheus.getFM, morpheus.getFile, result._3)
                    StatsCan.addStat(file, AffectedFeatures, result._3)
                    val time = new StopClock
                    // run refactored first
                    BusyBoxVerification.singleVerify(file, fm, "_ref")
                    runScript("./cleanAndReset.sh", sourcePath)
                    BusyBoxVerification.singleVerify(file, fm, "_org")
                    runScript("./cleanAndReset.sh", sourcePath)
                    StatsCan.addStat(file, TestingTime, time.getTime)
                } else writeError("Could not engine file.", path)
                val writer = new FileWriter(path + ".stats")
                StatsCan.write(writer)
                writer.flush()
                writer.close()
            } catch {
                case e: Exception => {
                    e.printStackTrace
                    writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), new File(path).getCanonicalPath)
                }
            }
        }
    }

}



