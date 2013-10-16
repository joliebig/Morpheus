package de.fosd.typechef.crefactor.evaluation

import scala.collection.mutable

object StatsJar {

    lazy val statsJar = new mutable.HashMap[String, mutable.Map[Stats.Value, Any]]()

    def addStat(file: String, stat: Stats.Value, value: Any) = {
        statsJar.get(file) match {
            case Some(x) => x += (stat -> value)
            case None => statsJar += (file -> mutable.HashMap(stat -> value))
        }
    }

    def write(filePath: String) =
     
    {

    }

}

object Stats extends Enumeration {

    val Parsing, TypeCheck, RandomRefactorDeterminationTime, PrettyPrint, RenamedId, Amount, RefactorTime, AffectedFeatures = Value

}
