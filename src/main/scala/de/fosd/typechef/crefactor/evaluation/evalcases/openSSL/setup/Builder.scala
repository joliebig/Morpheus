package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.setup

import de.fosd.typechef.crefactor.evaluation.setup.Building
import de.fosd.typechef.parser.c.AST
import java.io.File
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.OpenSSLEvaluation
import de.fosd.typechef.featureexpr.{SingleFeatureExpr, FeatureModel}
import java.nio.file.{Paths, Files}
import scala.io.Source


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

        def buildAndTest(file: File, ext: String): Boolean = {
            val dir = new File(resultDir.getCanonicalPath + "/" + ext)
            if (!dir.exists) dir.mkdirs

            val buildResult = build
            val testResult = test
            writeResult(buildResult._2, dir.getCanonicalPath + "/" + file.getName + ".build")
            if (!buildResult._1) writeResult(buildResult._3, dir.getCanonicalPath + "/" + file.getName + ".buildErr")
            writeResult(testResult._2 + "\n" + testResult._3, dir.getCanonicalPath + "/" + file.getName + ".test")
            buildResult._1 && testResult._1
        }

        var confNum = 0

        val pairWiseOrgBuilds = pairWiseConfigs._1.map(conf => {
            confNum += 1

            val dir = new File(resultDir.getCanonicalPath + "/" + confNum)
            if (!dir.exists) dir.mkdirs

            def filterNoFeature(feature: SingleFeatureExpr, trueRet: Option[String], falseRet: Option[String]): Iterable[String] = {
                if (feature.feature.startsWith(openSSLNoFeaturePrefix)) trueRet
                else falseRet
            }

            val configureArguments = buildSystem + " " + conf.getTrueSet.flatten(feature =>
                filterNoFeature(feature, Some(feature.feature.replace(openSSLFeaturePrefix, "").replace("_", "-").toLowerCase), None)).mkString(" ")

            val compilerDArgs = conf.getTrueSet.flatten(feature =>
                filterNoFeature(feature, Some(feature.feature), None))

            val compilerUArgs = conf.getFalseSet.flatten(feature =>
                filterNoFeature(feature, None, Some(feature.feature)))


            val configure = runScript("./Configure", sourcePath, configureArguments, runTimeout)
            val confStrings = streamsToString(configure)

            writeResult(confStrings._1, dir.getCanonicalPath + "/configureStd")
            writeResult(confStrings._2, dir.getCanonicalPath + "/configureErr")
            writeResult(conf.toString, dir.getCanonicalPath + "/config")

            val makeFile = new File(sourcePath + "/Makefile")
            val confFile = new File(sourcePath + "/crypto/opensslconf.h")

            Files.copy(Paths.get(makeFile.toURI), Paths.get(dir.getCanonicalPath + "/Makefile"))
            Files.copy(Paths.get(confFile.toURI), Paths.get(dir.getCanonicalPath + "/opensslconf.h"))

            val makeDep = runScript("./makedep.sh", sourcePath)
            val makeDepStrings = streamsToString(makeDep)

            writeResult(makeDepStrings._1, dir.getCanonicalPath + "/makedepStd")
            writeResult(makeDepStrings._2, dir.getCanonicalPath + "/makedepErr")


            val fileLines = Source.fromFile(makeFile).getLines.toList
            val customMakeFile = fileLines.map(line => {
                if (line.startsWith("CFLAG=")) addOwnGCCcmds("CFLAG=", line, compilerDArgs, compilerUArgs)
                else line
            }).mkString

            writeResult(customMakeFile, makeFile.getCanonicalPath)

            val btRes = buildAndTest(currentFile, "_org" + confNum)

            runScript("./clean.sh", sourcePath)
            (conf, btRes)
        })

        println("+++ Pairwisebuilds:")
        println(pairWiseOrgBuilds)

        val result = pairWiseOrgBuilds.map(res => res._2.toString + "\t" + res._1.toString)
        writeResult(result.mkString("\n"), resultDir + "/pairWiseBuildTest")

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
