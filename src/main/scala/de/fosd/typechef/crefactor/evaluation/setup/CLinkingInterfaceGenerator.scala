package de.fosd.typechef.crefactor.evaluation.setup

import de.fosd.typechef.crefactor.evaluation.Evaluation
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}
import de.fosd.typechef.typesystem.linker.{EmptyInterface, CInterface, SystemLinker, InterfaceWriter}
import java.io.File

/**
 * Interface for generating linking informations of a whole given project
 */
trait CLinkingInterfaceGenerator extends Evaluation with App {

    val fileList = io.Source.fromFile(filesToEval).getLines().toList

    private def getFMConstraints: Iterator[FeatureExpr] =
        for (l: String <- io.Source.fromFile(featureModel).getLines(); if l != "")
        yield new FeatureExprParser().parse(l)

    val fm_constraints = getFMConstraints.fold(FeatureExprFactory.True)(_ and _)
    val fm = FeatureExprFactory.default.featureModelFactory.create(fm_constraints)
    val reader = new InterfaceWriter() {}

    println(fileList.size + " files to analyse for linking informations.")

    val interfaces = fileList.map(f => reader.readInterface(new File(sourcePath + f + ".interface"))).map(SystemLinker.linkStdLib)

    private def linkTreewise(l: List[CInterface]): CInterface = {
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
                    println(c + " is not a tautology in feature model.")
            if (!(left isCompatibleTo right))
                println(confl + " is not compatible with feature model.")
            left debug_join right
        } else if (l.size == 1) l(0)
        else {
            assert(false, l)
            EmptyInterface
        }

    }

    private def linkIncrementally(l: List[CInterface]): CInterface = l.fold(EmptyInterface)((left, right) => {
        if (!(left isCompatibleTo right))
            println("Conflict: " + (left getConflicts right))
        left debug_join right
    })

    val finalInterface = linkIncrementally(interfaces).pack

    println("Exports: " + finalInterface.exports)
    println("Imports: " + finalInterface.imports)

    reader.writeInterface(finalInterface, new File(completePath + "/linking.interface"))
    reader.debugInterface(finalInterface, new File(completePath + "/linking.dbginterface"))

    println("Generated linkinterfaces.")

    new CLinking(completePath + "/linking.interface")

}
