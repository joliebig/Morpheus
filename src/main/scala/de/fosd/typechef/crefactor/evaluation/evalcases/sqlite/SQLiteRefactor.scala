package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.sqlite.SQLiteEvaluation
import de.fosd.typechef.crefactor.evaluation.{StatsJar, Refactoring, Refactor}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel
import java.io.File
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.refactor.{Extract, Inline, Rename}
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.evaluation.setup.CLinking


object SQLiteRefactor extends SQLiteEvaluation with Refactor {
    def rename(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = evaluate(ast, fm, file, Rename)
    def inline(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = evaluate(ast, fm, file, Inline)
    def extract(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking) = evaluate(ast, fm, file, Extract)

    private def evaluate(ast: AST, fm: FeatureModel, file: String, r: Refactoring): Unit = {
        println("+++ File to refactor: " + getFileName(file) + " +++")
        val resultDir = getResultDir(file)
        val path = resultDir.getCanonicalPath + File.separatorChar
        val resDir = new File(path)
        resDir.mkdirs()
        if (ast == null) println("+++ AST is null! +++")
        else if (blackListFiles.exists(getFileName(file).equalsIgnoreCase)) println("+++ File is blacklisted and cannot be build +++")
        else {
            try {
                val morpheus = new Morpheus(ast, fm, file)
                // reset test environment
                runScript("./clean.sh", sourcePath)
                val result = r.refactor(morpheus)
                if (result._1) {
                    write(result._2, morpheus.getFile)
                    val time = new TimeMeasurement
                    StatsJar.addStat(file, AffectedFeatures, result._2)
                    SQLiteVerification.verify(morpheus.getFile, morpheus.getFeatureModel, "first")
                    runScript("./clean.sh", sourcePath)
                    StatsJar.addStat(file, TestingTime, time.getTime)
                } else writeError("Could not refactor file.", path)
                StatsJar.write(path + ".stats")
            } catch {
                case e: Exception => {
                    e.printStackTrace
                    writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), resDir.getCanonicalPath)
                }
            }
        }
    }
}
