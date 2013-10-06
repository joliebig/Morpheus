package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.setup.building

import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.BusyBoxEvaluation

object Builder extends BusyBoxEvaluation with App {

    val fileList = io.Source.fromFile(busyBoxFiles).getLines().toList

    val blackWhiteList = fileList.foldLeft((List[String](), List[String]()))((lists, file) => lists)

}
