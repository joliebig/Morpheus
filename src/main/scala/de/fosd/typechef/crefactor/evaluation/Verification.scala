package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.featureexpr.FeatureModel
import java.io.{InputStreamReader, BufferedReader, InputStream}

trait Verification {

    def verify(evalFile: String, fm: FeatureModel, mode: String)

    def evaluateScriptResult(result: (InputStream, InputStream)): (Boolean, String, String) = {
        val stream = streamsToString(result)
        println("+++ STDOUT")
        println(stream._1)
        println("+++ STDERR")
        println(stream._2)
        (stream._1.contains("Success_Build"), stream._1, stream._2)
    }

    def streamsToString(streams: (InputStream, InputStream)): (String, String) = {
        val readerOut = new BufferedReader(new InputStreamReader(streams._1))
        val readerErr = new BufferedReader(new InputStreamReader(streams._2))

        val out = readIn(readerOut, new StringBuilder)
        val err = readIn(readerErr, new StringBuilder)
        (out, err)
    }

    def readIn(reader: BufferedReader, builder: StringBuilder): String = {
        while (reader.ready()) {
            val line = reader.readLine()
            builder.append(line + "\n")
        }
        builder.toString()
    }


}
