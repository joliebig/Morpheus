package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.sqlite.SQLiteEvaluation
import de.fosd.typechef.crefactor.evaluation.Verification
import de.fosd.typechef.featureexpr.FeatureModel
import scala.Predef._
import scala.Predef.String
import java.io.{File, InputStream}


object SQLiteVerification extends SQLiteEvaluation with Verification {

    def verify(evalFile: String, fm: FeatureModel, mode: String) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result"))

        val testResult = test
        val buildResult = build

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + mode + "/" + ".build")
        if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + mode + "/" + ".buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + mode + "/" + ".test")
        if (!testResult._1) writeResult(testResult._3, resultDir.getCanonicalPath + "/" + mode + "/" + ".testError")

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + mode + "/" + ".result")
    }

    private def test: (Boolean, String, String) = {
        val result = runScript("./testTH3.sh", th3Path)
        evaluateScriptResult(result)
    }

    private def build: (Boolean, String, String) = {
        val result = runScript("./buildSQLite.sh", sourcePath)
        evaluateScriptResult(result)
    }


    private def evaluateScriptResult(result: (InputStream, InputStream)): (Boolean, String, String) = {
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }
}
