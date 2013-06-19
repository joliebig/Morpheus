package de.fosd.typechef.crefactor.BusyBoxEvaluation

import java.io.{InputStreamReader, BufferedReader, File}
import de.fosd.typechef.featureexpr.FeatureModel
import org.junit.Test
import de.fosd.typechef.crefactor.util.EvalHelper


trait BusyBoxEvaluation extends EvalHelper {

    val FORCE_VARIABILITY = true
    val MAX_DEPTH = 27

    val amountOfRefactorings = 3

    @Test
    def evaluate()
}


object RefactorVerification extends EvalHelper {

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean = {
        val orgFile = bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched")
        val refFile = bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + bbFile.getName
        println(bbFile.getCanonicalPath)
        println(refFile)
        // First Build normal busybox
        buildBusyBox
        // TODO Test
        println(bbFile.getCanonicalPath)
        true
    }

    def buildBusyBox: Boolean = {
        var error = false
        val pb = new ProcessBuilder("./buildBusyBox.sh")
        pb.directory(new File(busyBoxPath))
        val p = pb.start()
        p.waitFor()

        val reader = new BufferedReader(new InputStreamReader(p.getInputStream()))
        while (reader.ready()) {
            println(reader.readLine())
        }

        val reader2 = new BufferedReader(new InputStreamReader(p.getErrorStream()))
        while (reader2.ready()) {
            error = true
            println(reader2.readLine())
        }
        !error
    }
}
