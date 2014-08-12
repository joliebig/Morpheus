package de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.refactor

import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.PreparedRefactorings
import de.fosd.typechef.crefactor.evaluation.evalcases.openSSL.OpenSSLEvaluation
import de.fosd.typechef.crefactor.evaluation.defaultEngines.DefaultRenameEngine
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.parser.c.TranslationUnit


object Rename extends OpenSSLEvaluation with DefaultRenameEngine {

    override def refactor(morpheus: Morpheus, preparedRefs : PreparedRefactorings):
    (Boolean, TranslationUnit, List[List[FeatureExpr]], List[(String, TranslationUnit)]) = {
        // clean prepared refactorings from blacklisted case study specific identifier
        val preparedCleanedRefactorings =
            preparedRefs.copy(renaming = preparedRefs.renaming.par.
                filter(id => !blackListNames.exists(_.equalsIgnoreCase(id.name))).toList)

        super.refactor(morpheus, preparedCleanedRefactorings)
    }

}
