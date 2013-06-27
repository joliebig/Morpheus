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

    def readIn(reader: BufferedReader, builder: StringBuilder): String = {
        while (reader.ready()) {
            val line = reader.readLine()
            builder.append(line)
        }
        builder.toString()
    }

    def runScript(script: String, dir: String): (InputStream, InputStream) = {
        val pb = new ProcessBuilder(script)
        pb.directory(new File(dir))
        val p = pb.start()
        p.waitFor()
        (p.getInputStream, p.getErrorStream)
    }

    def runTest: String = {
        val result = runScript("./runtest", busyBoxPath + "testsuite/")
        val stream = streamsToString(result)
        stream._1 + "\n" + stream._2
    }

    def buildBusyBox: (Boolean, String, String) = {
        val result = runScript("./buildBusyBox.sh", busyBoxPath)
        val stream = streamsToString(result)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

    private def streamsToString(streams: (InputStream, InputStream)): (String, String) = {
        val readerOut = new BufferedReader(new InputStreamReader(streams._1))
        val readerErr = new BufferedReader(new InputStreamReader(streams._2))

        val out = readIn(readerOut, new StringBuilder)
        val err = readIn(readerErr, new StringBuilder)
        (out, err)
    }
}
