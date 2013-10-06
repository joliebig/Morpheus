package de.fosd.typechef.crefactor.linking

import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.BusyBoxEvaluation
import java.io.File
import de.fosd.typechef.typesystem.linker.{EmptyInterface, CInterface, InterfaceWriter, SystemLinker}
import de.fosd.typechef.featureexpr.{FeatureExprParser, FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking.CLinking

object Linking extends BusyBoxEvaluation with App {

    val fileList = io.Source.fromFile(busyBoxFiles).getLines().toList
    val featureList = io.Source.fromFile(featuresFile).getLines().toList

    def getBusyboxVMConstraints: Iterator[FeatureExpr] =
        for (l: String <- io.Source.fromFile(featureModel).getLines(); if (l != ""))
        yield new FeatureExprParser().parse(l)

    val vm = getBusyboxVMConstraints.fold(FeatureExprFactory.True)(_ and _)
    val fm = FeatureExprFactory.default.featureModelFactory.create(vm)
    val reader = new InterfaceWriter() {}
    println(fileList.size + " files")

    val interfaces = fileList.map(f => reader.readInterface(new File(busyBoxPath + f + ".interface"))).map(SystemLinker.linkStdLib(_))

    def linkTreewise(l: List[CInterface]): CInterface = {
        if (l.size > 2) {
            val m: Int = l.size / 2
            val left = l.take(m)
            val right = l.drop(m)
            linkTreewise(List(linkTreewise(left), linkTreewise(right)))
        }
        else if (l.size == 2) {
            val left = l(0)
            val right = l(1)
            val confl = left getConflicts right
            for (c <- confl)
                if (!c._2.isTautology(fm))
                    println(c)
            if (!(left isCompatibleTo right))
                println(confl)
            left link right
        } else if (l.size == 1) l(0)
        else {
            assert(false, l)
            EmptyInterface
        }

    }

    def linkIncrementally(l: List[CInterface]): CInterface = l.fold(EmptyInterface)((left, right) => {
        if (!(left isCompatibleTo right))
            println(left getConflicts right)
        left link right
    })

    val finalInterface = linkTreewise(interfaces).andFM(vm).pack
    reader.writeInterface(finalInterface, new File("bboxLink.interface"))
    reader.debugInterface(finalInterface, new File("bboxLink.dbginterface"))

    new CLinking("bboxLink.interface")
}
