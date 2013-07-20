package de.fosd.typechef.crefactor.BusyBoxEvaluation

import de.fosd.typechef.crefactor.util.{TimeMeasurement, Refactor}
import org.junit.Test
import java.io.File
import de.fosd.typechef.crefactor.Morpheus

trait BusyBoxRefactor extends BusyBoxEvaluation with Refactor {

    @Test
    def evaluate() {
        val files = getBusyBoxFiles.reverse
        val refactor = files.map(file => {
            val bb_file = new File(busyBoxPath + file)
            try {
                var stats = List[Any]()
                val parseTypeCheckMs = new TimeMeasurement
                val parsed = parse(bb_file)
                val ast = parsed._1
                val fm = parsed._2
                val morpheus = new Morpheus(ast, fm)
                val parseTypeCheckTime = parseTypeCheckMs.getTime
                stats ::= parseTypeCheckTime
                runRefactor(morpheus, stats, bb_file, fm, 0, MAX)
            } catch {
                case e: Exception => {
                    println(e.getMessage)
                    println(e.getStackTrace.mkString("\n"))
                    writeExeception(e.getMessage + "\n" + e.getStackTrace.mkString("\n"), bb_file.getCanonicalPath, 0)
                    false
                }
            }
        })
        println("Refactor succ: " + refactor.contains(false))
        refactor.contains(false)
    }

}
