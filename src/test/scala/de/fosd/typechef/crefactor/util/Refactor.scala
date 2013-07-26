package de.fosd.typechef.crefactor.util

import de.fosd.typechef.crefactor.Morpheus
import java.io.File
import de.fosd.typechef.featureexpr.FeatureModel
import org.junit.Test

trait Refactor extends Evaluation {

    @Test
    def evaluate(file: String)

    def runRefactor(morpheus: Morpheus, stats: List[Any], bb_file: File, fm: FeatureModel, run: Int, max: Int, lastResult: Boolean = true): Boolean
}
