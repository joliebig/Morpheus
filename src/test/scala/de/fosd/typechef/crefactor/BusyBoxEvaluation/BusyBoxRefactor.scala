package de.fosd.typechef.crefactor.BusyBoxEvaluation

import de.fosd.typechef.crefactor.util.{TimeMeasurement, Refactor}
import org.junit.Test
import java.io.File
import de.fosd.typechef.crefactor.Morpheus

trait BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    @Test
    def evaluate() =
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
    }
}
