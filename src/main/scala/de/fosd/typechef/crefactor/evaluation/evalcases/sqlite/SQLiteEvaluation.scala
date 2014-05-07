package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.parser.c.{TranslationUnit, ConditionalNavigation, ASTNavigation}
import java.io.{FileWriter, File}
import de.fosd.typechef.crefactor.evaluation.{PreparedRefactorings, StatsCan, Refactoring, Evaluation}
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.refactor.{Inline, Extract, Rename}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.crefactor.backend.CModuleInterface
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.parser.c.TranslationUnit


trait SQLiteEvaluation extends Evaluation with ASTNavigation with ConditionalNavigation {

    val evalName = "sqlite"
    val caseStudyPath = "../cRefactor-SQLiteTH3Evaluation/"
    val completePath = new File(caseStudyPath).getCanonicalPath
    val filesToEval: String = completePath + "/sqlite_files"
    val evalFiles = getEvaluationFiles
    val blackListFiles: List[String] = List()
    val blackListIds: List[String] = List()
    val sourcePath = completePath + "/" + evalName + "/"
    val testPath = completePath + "/TH3/"
    val result = "/result/"

    val filterFeatures = List()
    val allFeaturesFile = completePath + "/allFeatures"
    val allFeatures = getAllFeaturesFromUniqueFeatureFile
    val pairWiseFeaturesFile = null
    val existingConfigsDir: String = completePath + "/existing_configs/"

    val featureModel: String = completePath + "/fm.txt"
    val featureModel_DIMACS: String = completePath + "/sqlite.dimacs"

    val runTimeout = 4800000

    val FORCE_VARIABILITY = true
    val FORCE_LINKING = false

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
        if (tunit == null) println("+++ AST is null! +++")
        else if (blackListFiles.exists(getFileName(file).equalsIgnoreCase)) println("+++ File is blacklisted and cannot be build +++")
        else {
            try {
                val morpheus = new Morpheus(tunit, fm, linkInterface, file)
                // reset test environment
                runScript("./clean.sh", sourcePath)
                val result = r.refactor(morpheus, preparedRefactorings)
                if (result._1) {
                    write(result._2, morpheus.getFile)
                    StatsCan.addStat(file, AffectedFeatures, result._2)
                    val affectedFeatureExpr = result._3.foldRight(List[FeatureExpr]()) {(l, c) => l ::: c}.distinct
                    logger.info("Starting verification.")
                    SQLiteVerification.completeVerify(morpheus.getFile, morpheus.getFM, affectedFeatureExpr)
                } else writeError("Could not engine file.", path)
                val writer = new FileWriter(path + ".stats")
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
