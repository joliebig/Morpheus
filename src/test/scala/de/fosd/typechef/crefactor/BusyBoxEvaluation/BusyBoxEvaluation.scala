package de.fosd.typechef.crefactor.BusyBoxEvaluation

import java.io._
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

    def copyFile(file1: File, file2: File) = {
        val src = file1
        val dest = file2
        new FileOutputStream(dest) getChannel() transferFrom(new FileInputStream(src) getChannel, 0, Long.MaxValue)
        println(dest.length())
    }

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean = {
        val verfiyPath = bbFile.getCanonicalPath
        val orgFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched"))
        val refFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + bbFile.getName)

        buildBusyBox
        println(bbFile.length())
        bbFile.delete()

        copyFile(refFile, new File(verfiyPath))

        // TODO Config
        // First Build normal busybox
        buildBusyBox
        // TODO Test
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
