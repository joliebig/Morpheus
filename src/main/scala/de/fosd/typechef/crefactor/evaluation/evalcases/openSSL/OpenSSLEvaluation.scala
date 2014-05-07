package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.{PreparedRefactorings, StatsCan, Refactoring, Evaluation}
import de.fosd.typechef.parser.c.{TranslationUnit, ConditionalNavigation, ASTNavigation}
import java.io.{FileWriter, File}
import scala.io.Source
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel, FeatureExprFactory}
import java.util.IdentityHashMap
import java.util
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.refactor.{Inline, Extract, Rename}
import de.fosd.typechef.crefactor.backend.CModuleInterface
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.parser.c.TranslationUnit


trait OpenSSLEvaluation extends Evaluation with ASTNavigation with ConditionalNavigation {

    val evalName = "openssl"
    val caseStudyPath = "../cRefactor-OpenSSLEvaluation/"
    val completePath = new File(caseStudyPath).getCanonicalPath
    val filesToEval: String = completePath + "/openssl_files"
    val evalFiles = getEvaluationFiles
    val blackListFiles: List[String] = List()
    val blackListIds: List[String] = List()
    //Source.fromFile(getClass.getResource("/openssl_id_blacklist").getFile).getLines().toList
    //Source.fromFile(getClass.getResource("/openssl_blacklist").getFile).getLines().toList
    val sourcePath = completePath + "/" + evalName + "/"
    val testPath = completePath + "/" + evalName + "/"
    val result = "/result/"

    val filterFeatures = Source.fromFile(new File(completePath + "/buildAbleNoFeatures")).getLines().toList
    val allFeaturesFile = completePath + "/allFeatures"
    val allFeatures = getAllFeaturesFromUniqueFeatureFile
    val pairWiseFeaturesFile = sourcePath + "/openssl_pairwise_configs.csv"
    val existingConfigsDir: String = completePath + "/existing_configs/"

    val featureModel: String = sourcePath + "/featuremodel"
    val featureModel_DIMACS: String = sourcePath + "/OpenSSL.dimacs"

    val runTimeout = 300000

    val FORCE_VARIABILITY = true
    val FORCE_LINKING = false

    val openSSLNoFeaturePrefix = "OPENSSL_NO"
    val openSSLFeaturePrefix = "OPENSSL_"
    val buildSystem = "linux-x86_64"

    val renameEngine = Rename
    val extractEngine = Extract
    val inlineEngine = Inline

    override def evaluate(preparedRefactorings : PreparedRefactorings, tunit: TranslationUnit, fm: FeatureModel,
                          file: String, linkInterface: CModuleInterface, r: Refactoring): Unit = {
        println("+++ File to engine: " + getFileName(file) + " +++")
        val resultDir = getResultDir(file)
        val path = resultDir.getCanonicalPath + File.separatorChar
        val resDir = new File(path)
        resDir.mkdirs()
        if (tunit == null) println("+++ TUNIT is null! +++")
        else if (blackListFiles.exists(getFileName(file).equalsIgnoreCase)) println("+++ File is blacklisted and cannot be build +++")
        else {
            try {
                val morpheus = new Morpheus(tunit, fm, linkInterface, file)
                // reset test environment
                runScript("./clean.sh", sourcePath)
                val result = r.refactor(morpheus, preparedRefactorings)
                if (result._1) {
                    write(result._2, morpheus.getFile.replace(".pi", ".c"))
                    StatsCan.addStat(file, AffectedFeatures, result._3)
                    val affectedFeatureExpr = result._3.foldRight(List[FeatureExpr]()) {(l, c) => l ::: c}.distinct
                    logger.info("Starting verification.")
                    OpenSSLVerification.completeVerify(morpheus.getFile, morpheus.getFM, affectedFeatureExpr)
                } else writeError("Could not engine file.", path)
                val writer = new FileWriter(path + getFileName(file) + ".stats")
                StatsCan.write(writer)
                writer.flush()
                writer.close()
            } catch {
                case e: Exception => {
                    e.printStackTrace()
                    writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), resDir.getCanonicalPath)
                }
            }
        }
    }
}
