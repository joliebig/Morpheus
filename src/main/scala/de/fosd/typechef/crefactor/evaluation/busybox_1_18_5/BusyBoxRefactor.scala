package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5

import de.fosd.typechef.crefactor.evaluation.{StatsJar, Refactor}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crefactor.Morpheus
import java.io.File
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking
import de.fosd.typechef.crefactor.evaluation.util.TimeMeasurement
import de.fosd.typechef.crefactor.evaluation.Stats._


trait BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    def evaluate(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking): Unit = {
        println("+++ Current File: " + file + " +++")
        val resultDir = getResultDir(file)
        val path = resultDir.getCanonicalPath + File.separatorChar + getFileName(file)
        if (ast == null) println("+++ AST is null! +++")
        else if (blackListFiles.exists(getFileName(file).equalsIgnoreCase)) println("+++ File is blacklisted and cannot be build +++")
        else {
            try {
                val morpheus = new Morpheus(ast, fm, file)
                // reset test environment
                runScript("./cleanAndReset.sh", busyBoxPath)
                val features = refactor(morpheus, linkInterface)
                if (features._1) {
                    val time = new TimeMeasurement
                    StatsJar.addStat(file, AffectedFeatures, features._2)
                    // run refactored first
                    BusyBoxVerification.verify(file, fm, "_ref")
                    runScript("./cleanAndReset.sh", busyBoxPath)
                    BusyBoxVerification.verify(file, fm, "_org")
                    runScript("./cleanAndReset.sh", busyBoxPath)
                    StatsJar.addStat(file, TestingTime, time.getTime)
                } else writeError("Could not refactor, cause possible bad linking.", path)
                StatsJar.write(path + ".stats")
            } catch {
                case e: Exception => {
                    println(e.getStackTrace.mkString("\n"))
                    writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), new File(path).getCanonicalPath)
                }
            }
        }
    }
}