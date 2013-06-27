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

    def copyFile(file1: File, file2: File) = new FileOutputStream(file2).getChannel() transferFrom(new FileInputStream(file1).getChannel, 0, Long.MaxValue)

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean = {
        val workingPath = bbFile.getCanonicalPath
        val orgFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "busybox-1.18.5_untouched"))
        val refFile = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/" + bbFile.getName)
        val verfiyDir = new File(bbFile.getCanonicalPath.replaceAll("busybox-1.18.5", "result") + "/" + run + "/")

        val configs = verfiyDir.listFiles(new FilenameFilter {
            def accept(input: File, file: String): Boolean = file.endsWith(".config")
        })

        configs.forall(config => {
            def buildAndTest(busyBoxFile: File, ext: String): String = {
                val buildResult = buildBusyBox
                val testResult = runTest
                writeResult(buildResult._2, verfiyDir.getCanonicalPath + "/" + config.getName + ext + ".buildAndTest")
                writeResult(testResult, verfiyDir.getCanonicalPath + "/" + config.getName + ext + ".test")
                bbFile.delete()
                testResult
            }

            val configBuild = new File(busyBoxPath + ".config")
            copyFile(config, configBuild)

            val orgTest = buildAndTest(bbFile, "_org")

            val buildRefFile = new File(workingPath)

            // Replace original file with refactored file
            copyFile(refFile, buildRefFile)

            val refTest = buildAndTest(buildRefFile, "_ref")

            // Restore old original file again
            copyFile(orgFile, new File(workingPath))
            configBuild.delete()

            writeResult(orgTest.equals(refTest).toString, verfiyDir.getCanonicalPath + "/" + config.getName + ".result")
            orgTest.equals(refTest)
        })
    }

    def writeResult(result: String, file: String) = {
        var out: FileWriter = null
        if (file.startsWith(".")) out = new java.io.FileWriter(file.replaceFirst(".", ""))
        else out = new java.io.FileWriter(file)

        out.write(result)
        out.flush()
        out.close()
    }

    def runTest: String = {
        var error = false
        val pb = new ProcessBuilder("./runtest")
        pb.directory(new File(busyBoxPath + "testsuite/"))
        val p = pb.start()
        p.waitFor()

        val reader = new BufferedReader(new InputStreamReader(p.getInputStream()))
        val sb = new StringBuilder
        while (reader.ready()) {
            val line = reader.readLine()
            sb.append(line)
            println(line)
        }

        val reader2 = new BufferedReader(new InputStreamReader(p.getErrorStream()))
        while (reader2.ready()) {
            error = true
            val line = reader2.readLine()
            sb.append(line)
            println(line)
        }
        sb.toString()
    }

    def buildBusyBox: (Boolean, String, String) = {
        def readIn(reader: BufferedReader, builder: StringBuilder): String = {
            while (reader.ready()) {
                val line = reader.readLine()
                builder.append(line)
            }
            builder.toString()
        }

        val pb = new ProcessBuilder("./buildBusyBox.sh")
        pb.directory(new File(busyBoxPath))
        val p = pb.start()
        p.waitFor()

        val readerOut = new BufferedReader(new InputStreamReader(p.getInputStream))
        val readerErr = new BufferedReader(new InputStreamReader(p.getErrorStream))

        val out: String = readIn(readerOut, new StringBuilder)
        val err: String = readIn(readerErr, new StringBuilder)

        (out.contains("Success_Build"), out, err)
    }
}
