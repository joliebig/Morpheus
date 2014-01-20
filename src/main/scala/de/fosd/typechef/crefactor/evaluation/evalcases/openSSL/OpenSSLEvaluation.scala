package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL

import de.fosd.typechef.crefactor.evaluation.Evaluation
import de.fosd.typechef.parser.c.{ConditionalNavigation, ASTNavigation}
import java.io.File


trait OpenSSLEvaluation extends Evaluation with ASTNavigation with ConditionalNavigation {

    val evalName = "openssl"
    val caseStudyPath = "../cRefactor-OpenSSLEvaluation/"
    val completePath = new File(caseStudyPath).getCanonicalPath
    val filesToEval: String = completePath + "/openssl_files"
    val blackListFiles: List[String] = List()
    //Source.fromFile(getClass.getResource("/openssl_blacklist").getFile).getLines().toList
    val sourcePath = completePath + "/" + evalName + "/"
    val testPath = completePath + "/" + evalName + "/"
    val result = "/result/"

    val filterFeatures = List()
    val allFeaturesFile = null
    val allFeatures = null
    val pairWiseFeaturesFile = caseStudyPath + "/openssl_pairwise_configs.csv"

    val featureModel: String = completePath + "/fm.txt"
    val featureModel_DIMACS: String = completePath + "/OpenSSL.dimacs"

    val runTimeout = 300000

    val FORCE_VARIABILITY = true
    val FORCE_LINKING = false


}
