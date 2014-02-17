package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.{StatsJar, Refactoring, Refactor}
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.FeatureModel
import java.io.File
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.refactor.{Inline, Extract, Rename}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.backend.CLinking

object OpenSSLRefactor extends OpenSSLEvaluation with Refactor {
    def rename(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking) =
        evaluate(tunit, fm, file, linkInterface, Rename)
    def extract(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking) =
        evaluate(tunit, fm, file, linkInterface, Extract)
    def inline(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking) =
        evaluate(tunit, fm, file, linkInterface, Inline)

    private def evaluate(tunit: TranslationUnit, fm: FeatureModel, file: String, linkInterface: CLinking, r: Refactoring): Unit = {
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
                val result = r.refactor(morpheus)
                if (result._1) {
                    write(result._2, morpheus.getFile.replace(".pi", ".c"))
                    val time = new StopClock
                    StatsJar.addStat(file, AffectedFeatures, result._2)
                    OpenSSLVerification.verify(morpheus.getFile, morpheus.getFM, "first")
                    runScript("./clean.sh", sourcePath)
                    StatsJar.addStat(file, TestingTime, time.getTime)
                } else writeError("Could not engine file.", path)
                StatsJar.write(path + ".stats")
            } catch {
                case e: Exception => {
                    e.printStackTrace()
                    writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), resDir.getCanonicalPath)
                }
            }
        }
    }
}
