package de.fosd.typechef.crefactor.evaluation

import de.fosd.typechef.crefactor.Morpheus
import java.io.File
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.typesystem.CTypeSystemFrontend

trait Refactor extends Evaluation {

    def evaluate(ast: AST, model: FeatureModel, ts: CTypeSystemFrontend, file: String, parsingTime: Long)

    def runRefactor(morpheus: Morpheus, stats: List[Any], bb_file: File, fm: FeatureModel, run: Int, max: Int, lastResult: Boolean = true): Boolean
}
