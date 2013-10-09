package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5

import de.fosd.typechef.crefactor.evaluation.Refactor
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import de.fosd.typechef.typesystem.CTypeSystemFrontend
import de.fosd.typechef.crefactor.Morpheus
import java.io.File
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking

trait BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    def evaluate(ast: AST, fm: FeatureModel, ts: CTypeSystemFrontend, file: String, parsingTime: Long, interface: CLinking = null): Unit = {
        println("+++ Current File: " + file + " +++")
        if (ast == null) println("+++ AST is null! +++")
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