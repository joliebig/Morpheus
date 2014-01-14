package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.FeatureModel
import java.io.File

trait Verification extends Evaluation {

    val buildScript: String = "./build.sh"
    val testScript: String = "./test.sh"
    val cleanScript: String = "./clean.sh"

    def verify(evalFile: String, fm: FeatureModel, mode: String) = {
        val resultDir = new File(evalFile.replaceAll(evalName, "result") + "/" + mode + "/")
        if (!resultDir.exists) resultDir.mkdirs

        val buildResult = build
        val testResult = test

        writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + "build")
        if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + "buildErr")

        writeResult(testResult._2, resultDir.getCanonicalPath + "/" + "test")
        if (!testResult._1) writeResult(testResult._3, resultDir.getCanonicalPath + "/" + "testError")

        writeResult((testResult._1 && buildResult._1).toString, resultDir.getCanonicalPath + "/" + "result")
    }

    def test: (Boolean, String, String) = {
        val result = runScript(testScript, testPath)
        evaluateScriptResult(result)
    }

    def build: (Boolean, String, String) = {
        val result = runScript(buildScript, sourcePath)
        evaluateScriptResult(result)
    }


}
