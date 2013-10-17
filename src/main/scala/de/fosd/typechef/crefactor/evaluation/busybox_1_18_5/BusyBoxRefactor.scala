package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5

import de.fosd.typechef.crefactor.evaluation.{StatsJar, Refactor}
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
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
        val morpheus = new Morpheus(ast, fm, file)
        try {
            // reset test environment
            runScript("./cleanAndReset.sh", busyBoxPath)
            val features = refactor(morpheus, linkInterface)
            // run refactored first
            val time = new TimeMeasurement
            BusyBoxVerification.verify(file, fm, "_ref")
            runScript("./cleanAndReset.sh", busyBoxPath)
            BusyBoxVerification.verify(file, fm, "_org")
            runScript("./cleanAndReset.sh", busyBoxPath)
            StatsJar.addStat(file, TestingTime, time.getTime)


            StatsJar.write(path + ".stats")
        } catch {
            case e: Exception => {
                println(e.getStackTrace.mkString("\n"))
                writeException(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), new File(path).getCanonicalPath)
            }
        }
    }

    // TODO toRemove
    def evalRefactoredAST(result: (AST, Boolean, List[FeatureExpr], List[Any]), bb_file: File, run: Int, morpheus: Morpheus, fm: FeatureModel): Boolean = {
        if (result._2) {
            val dir = getResultDir(bb_file.getCanonicalPath, run)
            val path = dir.getCanonicalPath + File.separatorChar + getFileName(bb_file.getCanonicalPath)
            writeAST(result._1, path)
            writePlainAST(result._1, path + ".ast")
            PrepareASTforVerification.makeConfigs(result._1, morpheus.getFeatureModel, bb_file.getCanonicalPath, result._3, run)
        }

        val verify = BusyBoxVerification.verify(bb_file, run, fm)
        var stat2 = result._4
        stat2 = stat2.::(result._2 + "\n" + verify)
        writeStats(stat2, bb_file.getCanonicalPath, run)
        verify
    }
}