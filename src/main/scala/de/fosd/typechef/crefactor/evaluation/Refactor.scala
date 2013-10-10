package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.crefactor.Morpheus
import java.io.File
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking

trait Refactor extends Evaluation {

    def evaluate(ast: AST, fm: FeatureModel, file: String, linkInterface: CLinking)

    def runRefactor(morpheus: Morpheus, stats: List[Any], bb_file: File, fm: FeatureModel, run: Int, max: Int, lastResult: Boolean = true): Boolean

    def refactor(morpheus: Morpheus, linkInterface: CLinking): Boolean
}
