package de.fosd.typechef.crefactor.evaluation

import scala.collection.mutable
import java.io.Writer

object StatsCan {

    lazy val statsCan = mutable.HashMap[String, mutable.HashMap[Int, mutable.Map[Stats.Value, Any]]]()

    def addStat(file: String, stat: Stats.Value, value: Any): Unit = addStat(file, -1, stat, value)

    def addStat(file: String, run: Int, stat: Stats.Value, value: Any): Unit = {
        statsCan.get(file) match {
            case Some(fileStats) =>
                fileStats.get(run) match {
                    case Some(runStats) => runStats += (stat -> value)
                    case _ => fileStats += (run -> mutable.HashMap(stat -> value))
                }
            case _ => statsCan += (file -> mutable.HashMap(run -> mutable.HashMap(stat -> value)))
        }
    }

    def write(writer: Writer) = {
        statsCan.keySet.foreach(file => {
            writer.write("+++ Begin " + file + " +++\n")

            statsCan.get(file) match {
                case Some(currFile) => currFile.keySet.toList.sorted.foreach(run => {

                    writer.write("\t+++ Begin " + run + " +++\n")
                    currFile.get(run) match {
                        case Some(runStats) => runStats.keySet.foreach(stat => runStats.get(stat) match {
                            case Some(y) => writer.write("\t" + stat.toString + "\t" + y.toString + "\n")
                            case _ =>
                        })
                        case _ =>
                    }
                    writer.write("\t+++ End " + run + " +++\n")

                })
                case _ =>
            }

            writer.write("+++ End " + file + " +++\n")
        })
    }
}

object Stats extends Enumeration {

    val Parsing, TypeCheck, InlinedFunction, RandomRefactorDeterminationTime, PrettyPrint, RenamedId, Amount, RefactorTime, AffectedFeatures, TestingTime, Variants, Statements, ExternalUses, ExternalDecls, Parameters, Liveness, Type = Value

}
