package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5

import de.fosd.typechef.crefactor.evaluation.{Refactoring, StatsCan, Refactor}
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.crefactor.Morpheus
import java.io.{FileWriter, File}
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.Stats._
import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.refactor.{Rename, Inline, Extract}
import de.fosd.typechef.crefactor.backend.CModuleInterface


object BusyBoxRefactorEvaluation extends BusyBoxEvaluation with Refactor {

}