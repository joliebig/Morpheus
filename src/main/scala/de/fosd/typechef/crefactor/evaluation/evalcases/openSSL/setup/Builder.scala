package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup

import de.fosd.typechef.crefactor.evaluation.setup.Building
import de.fosd.typechef.parser.c.AST
import java.io.File
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.OpenSSLEvaluation
import de.fosd.typechef.featureexpr.{SingleFeatureExpr, FeatureModel}


object Builder extends OpenSSLEvaluation with Building {

    def canBuild(ast: AST, fm: FeatureModel, file: String): Boolean = {
        val currentFile = new File(file)
        // clean dir first
        runScript("./clean.sh", sourcePath)

        val refFile = new File(currentFile.getCanonicalPath.replace(evalName, "result") + "/" + currentFile.getName)
        val resultDir = new File(currentFile.getCanonicalPath.replace(evalName, "result") + "/")
        resultDir.mkdirs()

        initializeFeatureList(ast)
        val pairWiseConfigs = loadConfigurationsFromCSVFile(new File(pairWiseFeaturesFile), new File(featureModel_DIMACS), features, fm)

        def buildAndTest(busyBoxFile: File, ext: String): Boolean = {
            val buildResult = build
            val testResult = test
            writeResult(buildResult._2, resultDir.getCanonicalPath + "/" + ext + ".build")
            if (!buildResult._1) writeResult(buildResult._3, resultDir.getCanonicalPath + "/" + ext + ".buildErr")
            writeResult(testResult._2 + "\n" + testResult._3, resultDir.getCanonicalPath + "/" + ext + ".test")
            buildResult._1 && testResult._1
        }

        var confNum = 0

        val pairWiseOrgBuilds = pairWiseConfigs._1.map(conf => {
            confNum += 1
            def filterNoFeature(feature: SingleFeatureExpr, trueRet: Option[String], falseRet: Option[String]): Iterable[String] = {
                if (feature.feature.startsWith(openSSLNoFeaturePrefix)) trueRet
                else falseRet
            }

            val configureArguments = conf.getTrueSet.flatten(feature =>
                filterNoFeature(feature, Some(feature.feature.replace(openSSLFeaturePrefix, "").replace("_", "-").toLowerCase), None)) mkString (" ")

            val compilerDArgsList = conf.getTrueSet.flatten(feature =>
                filterNoFeature(feature, Some(feature.feature), None))

            println(configureArguments)

            val compilerUArgs = conf.getFalseSet.flatten(feature =>
                filterNoFeature(feature, None, Some(feature.feature)))


            val configure = runScript("./Configure", sourcePath, configureArguments, runTimeout)
            val btRes = buildAndTest(currentFile, "_org" + confNum)

            // TODO
            // -> save openconf.h & Makefile
            // add compiler options to compiler
            // make depend
            // make
            // make test
            // clean
            (conf, btRes)
        })

        println(pairWiseOrgBuilds)

        val org = buildAndTest(currentFile, "_org")
        runScript("./clean.sh", sourcePath)





        // write AST in current result dir
        printAndWriteAST(ast, refFile.getCanonicalPath)
        println("+++ Saving result to: " + refFile.getPath)
        println("+++ Updating file: " + currentFile.getCanonicalPath.replace(".pi", ".c"))
        printAndWriteAST(ast, currentFile.getCanonicalPath.replace(".pi", ".c"))

        val ppp = buildAndTest(currentFile, "_ppp")

        // clean dir
        runScript("./clean.sh", sourcePath)

        ppp && org
    }


    private def build: (Boolean, String, String) = {
        println("+++ Building")
        val result = runScript("./build.sh", sourcePath)
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

    private def test: (Boolean, String, String) = {
        println("+++ Testing")
        val result = runScript("./runtest.sh", sourcePath)
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Test"), stream._1, stream._2)
    }
}
