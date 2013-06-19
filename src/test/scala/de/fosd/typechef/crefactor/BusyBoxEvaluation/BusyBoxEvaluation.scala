package de.fosd.typechef.crefactor.BusyBoxEvaluation

import java.io.File
import de.fosd.typechef.featureexpr.FeatureModel
import org.junit.Test
import de.fosd.typechef.crefactor.util.EvalHelper


trait BusyBoxEvaluation extends EvalHelper {

    val FORCE_VARIABILITY = true
    val MAX_DEPTH = 27

    val amountOfRefactorings = 3

    @Test
    def evaluate()
}


object RefactorVerification {

    def verify(refactored: File, originalFile: File, run: Int, fm: FeatureModel): Boolean = {

    }
}
