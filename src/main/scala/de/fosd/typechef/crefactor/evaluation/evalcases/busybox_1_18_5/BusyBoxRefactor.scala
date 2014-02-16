package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5

import de.fosd.typechef.crefactor.evaluation.{Refactoring, StatsJar, Refactor}
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crefactor.Morpheus
import java.io.File
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.refactor.{Rename, Inline, Extract}
import de.fosd.typechef.crefactor.backend.CLinking


object BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    def rename(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking) =
        evaluate(tunit, fm, file, linkInterface, Rename)
    def extract(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking) =
        evaluate(tunit, fm, file, linkInterface, Extract)
    def inline(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking) =
        evaluate(tunit, fm, file, linkInterface, Inline)

    private def evaluate(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking, r: Refactoring): Unit = {
        logger.info("File to refactor: " + getFileName(file) + " +++")
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
                    PrepareASTforVerification.makeConfigs(result._2, morpheus.getFM, morpheus.getFile, result._3)
                    val time = new StopClock
                    StatsJar.addStat(file, AffectedFeatures, result._3)
                    // run refactored first
                    BusyBoxVerification.verify(file, fm, "_ref")
                    runScript("./cleanAndReset.sh", sourcePath)
                    BusyBoxVerification.verify(file, fm, "_org")
                    runScript("./cleanAndReset.sh", sourcePath)
                    StatsJar.addStat(file, TestingTime, time.getTime)
                } else writeError("Could not refactor file.", path)
                StatsJar.write(path + ".stats")
            } catch {
                case e: Exception => {
                    e.printStackTrace
                    writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), new File(path).getCanonicalPath)
                }
            }
        }
    }
}