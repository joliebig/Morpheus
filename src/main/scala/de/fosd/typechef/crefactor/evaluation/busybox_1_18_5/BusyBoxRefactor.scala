package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5

import java.io.File
import de.fosd.typechef.crefactor.evaluation.Refactor
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.typesystem.CTypeSystemFrontend

trait BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    /*def evaluate(): Unit = {

        getBusyBoxFiles.reverse.foreach(file => println(file))
        getBusyBoxFiles.reverse.foreach(file => {
            val bb_file = new File(busyBoxPath + file)
            try {
                runEval(bb_file)
            } catch {
                case e: Exception => {
                    println(e.getCause.toString)
                    println(e.getMessage)
                    println(e.getStackTrace.mkString("\n"))
                    writeExeception(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), bb_file.getCanonicalPath, -1)

                }
            }
        })
    } */

    def evaluate(ast: AST, model: FeatureModel, ts: CTypeSystemFrontend, file: String, parsingTime: Long): Unit = {
        println(file)
        println(allFeaturesFile)
        println(busyBoxPath)
        println(new File(busyBoxPathUntouched).getCanonicalPath)
        println(new File(allFeaturesFile).exists())
    }


    /*
    private def runEval(bb_file: File): Boolean = {
        var ref_result: Boolean = false
        try {
            var stats = List[Any]()
            val parseTypeCheckMs = new TimeMeasurement
            val parsed = parse(bb_file)
            val ast = parsed._1
            val fm = parsed._2
            val morpheus = new Morpheus(ast, fm)
            val parseTypeCheckTime = parseTypeCheckMs.getTime
            stats ::= parseTypeCheckTime
            ref_result = runRefactor(morpheus, stats, bb_file, fm, 0, MAX)
        } catch {
            case e: Exception => {
                println(e.getCause.toString)
                println(e.getMessage)
                println(e.getStackTrace.mkString("\n"))
                writeExeception(e.getCause.toString + "\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"), bb_file.getCanonicalPath, 0)
                ref_result = false
            }
        }
        ref_result
    }    */
}
