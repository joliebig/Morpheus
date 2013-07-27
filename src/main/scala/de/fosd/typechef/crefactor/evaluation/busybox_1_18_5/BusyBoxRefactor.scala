package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5

import de.fosd.typechef.crefactor.evaluation.Refactor
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.crefactor.Morpheus
import java.io.File

trait BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    def evaluate(ast: AST, fm: FeatureModel, ts: CTypeSystemFrontend, file: String, parsingTime: Long): Unit = {
        println("+++ Current File: " + file + " +++")
        println(busyBoxPathUntouched)

        val bb_file = new File(file)
        val stats = List[Any](parsingTime)
        val morpheus = new Morpheus(ast, fm)

        try {
            runRefactor(morpheus, stats, bb_file, fm, 0, MAX)
        } catch {
            case e: Exception => {
                println(e.getCause.toString)
                println(e.getMessage)
                println(e.getStackTrace.mkString("\n"))
                writeExeception(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), bb_file.getCanonicalPath, -1)
            }
        }
    }
}