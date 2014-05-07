package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.{StatsCan, Refactoring, Refactor}
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import java.io.{FileWriter, File}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.refactor.{Inline, Extract, Rename}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.backend.CModuleInterface

object OpenSSLRefactor extends OpenSSLEvaluation with Refactor {
    override def rename(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CModuleInterface) =
        evaluate(tunit, fm, file, linkInterface, Rename)
    override def extract(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CModuleInterface) =
        evaluate(tunit, fm, file, linkInterface, Extract)
    override def inline(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CModuleInterface) =
        evaluate(tunit, fm, file, linkInterface, Inline)

    override def prepareForEvaluation(tunit: TranslationUnit, fm: FeatureModel, file: String,
                                      linkInterface: CModuleInterface) = {

    }

    private def evaluate(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CModuleInterface, r: Refactoring): Unit = {
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
                val result = r.refactor(morpheus)
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
