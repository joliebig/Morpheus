package de.fosd.typechef.crefactor.evaluation.setup

import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.FeatureModel

trait Building {

    def canBuild(tunit: TranslationUnit, fm: FeatureModel, file: String): Boolean
}
