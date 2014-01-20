package de.fosd.typechef.crefactor.evaluation.setup

import de.fosd.typechef.parser.c.AST
import de.fosd.typechef.featureexpr.FeatureModel

trait Building {

    def canBuild(ast: AST, fm: FeatureModel, file: String): Boolean


}
