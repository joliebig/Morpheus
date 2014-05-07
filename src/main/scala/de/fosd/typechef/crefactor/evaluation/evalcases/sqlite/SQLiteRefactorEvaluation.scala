package de.fosd.typechef.crefactor.evaluation.evalcases.sqlite

import de.fosd.typechef.crefactor.evaluation.{StatsCan, Refactoring, Refactor}
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureModel}
import java.io.{FileWriter, File}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.evaluation.evalcases.sqlite.refactor.{Inline, Extract, Rename}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.backend.CModuleInterface


object SQLiteRefactorEvaluation extends SQLiteEvaluation with Refactor {

}
