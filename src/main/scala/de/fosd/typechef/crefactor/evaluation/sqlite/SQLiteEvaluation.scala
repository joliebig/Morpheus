package de.fosd.typechef.crefactor.evaluation.sqlite

import de.fosd.typechef.parser.c.{ConditionalNavigation, ASTNavigation}
import java.io.File
import de.fosd.typechef.crefactor.evaluation.Evaluation


trait SQLiteEvaluation extends Evaluation with ASTNavigation with ConditionalNavigation {

    val evalName = "sqlite"
    val caseStudyPath = "../cRefactor-SQLiteEvaluation/"
    val completePath = new File(caseStudyPath).getCanonicalPath
    val filesToEval: String = completePath + "/sqlite_files"
    val blackListFiles: List[String] = List()
    //Source.fromFile(getClass.getResource("/openssl_blacklist").getFile).getLines().toList
    val sourcePath = completePath + "/" + evalName + "/"
    val result = "/result/"

    val filterFeatures = List()
    val allFeaturesFile = null
    val allFeatures = null
    val pairWiseFeaturesFile = null

    val featureModel: String = completePath + "/fm.txt"
    val featureModel_DIMACS: String = completePath + "/sqlite.dimacs"

    val runTimeout = 300000

    val FORCE_VARIABILITY = true
    val FORCE_LINKING = false


}
