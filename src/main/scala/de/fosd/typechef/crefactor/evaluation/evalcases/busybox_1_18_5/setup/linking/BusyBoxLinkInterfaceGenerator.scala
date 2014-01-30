package de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.setup.linking

import de.fosd.typechef.crefactor.evaluation.evalcases.busybox_1_18_5.BusyBoxEvaluation
import de.fosd.typechef.crefactor.evaluation.setup.CLinkingInterfaceGenerator

/**
 * Singleton instance for generating linking informations of a whole given project. This is one is for BusyBox.
 */
object BusyBoxLinkInterfaceGenerator extends CLinkingInterfaceGenerator with BusyBoxEvaluation {}
