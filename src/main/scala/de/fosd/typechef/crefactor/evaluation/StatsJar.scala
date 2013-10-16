package de.fosd.typechef.crefactor.evaluation

import scala.collection.mutable
import java.io.FileWriter

object StatsJar {

    lazy val statsJar = new mutable.HashMap[String, mutable.Map[Stats.Value, Any]]()

    def addStat(file: String, stat: Stats.Value, value: Any) = {
        statsJar.get(file) match {
            case Some(x) => x += (stat -> value)
            case None => statsJar += (file -> mutable.HashMap(stat -> value))
        }
    }

    def write(filePath: String) = {
        val writer = new FileWriter(filePath)
        statsJar.keySet.foreach(key => {
            writer.write("+++ " + key + " +++")
            writer.write("\n")
            statsJar.get(key) match {
                case Some(x) => x.keySet.foreach(stat => x.get(stat) match {
                    case Some(y) => writer.write(stat.toString + "\t" + y.toString + "\n")
                    case _ =>
                })
                case _ =>
            }
            writer.write("===\n")
        })
        writer.flush()
        writer.close()
    }

}

object Stats extends Enumeration {

    val Parsing, TypeCheck, RandomRefactorDeterminationTime, PrettyPrint, RenamedId, Amount, RefactorTime, AffectedFeatures = Value

}
