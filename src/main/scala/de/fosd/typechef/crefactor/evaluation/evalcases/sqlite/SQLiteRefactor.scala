package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.sqlite.SQLiteEvaluation
import de.fosd.typechef.crefactor.evaluation.{StatsJar, Refactoring, Refactor}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import java.io.File
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.refactor.{Extract, Inline, Rename}


object SQLiteRefactor extends SQLiteEvaluation with Refactor {
    def rename(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = evaluate(ast, fm, file, Rename)
    def inline(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = evaluate(ast, fm, file, Inline)
    def extract(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = evaluate(ast, fm, file, Extract)

    private def evaluate(ast: AST, fm: FeatureModel, file: String, r: Refactoring): Unit = {
        println("+++ File to refactor: " + getFileName(file) + " +++")
        val resultDir = getResultDir(file)
        val path = resultDir.getCanonicalPath + File.separatorChar + getFileName(file)
        if (ast == null) println("+++ AST is null! +++")
        else if (blackListFiles.exists(getFileName(file).equalsIgnoreCase)) println("+++ File is blacklisted and cannot be build +++")
        else {
            try {
                val morpheus = new Morpheus(ast, fm, file)
                // reset test environment
                runScript("./cleanAndReset.sh", sourcePath)
                val result = r.refactor(morpheus)
                if (result._1) {
                    write(result._2, morpheus.getFile)
                    /*// PrepareASTforVerification.makeConfigs(result._2, morpheus.getFeatureModel, morpheus.getFile, features)
                    val time = new TimeMeasurement
                    StatsJar.addStat(file, AffectedFeatures, result._2)
                    // run refactored first
                    BusyBoxVerification.verify(file, fm, "_ref")
                    runScript("./cleanAndReset.sh", sourcePath)
                    BusyBoxVerification.verify(file, fm, "_org")
                    runScript("./cleanAndReset.sh", sourcePath)
                    StatsJar.addStat(file, TestingTime, time.getTime)   */
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
