package de.fosd.typechef.crefactor.evaluation.setup

import de.fosd.typechef.crefactor.evaluation.Evaluation
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}
import de.fosd.typechef.typesystem.linker.{EmptyInterface, CInterface, SystemLinker, InterfaceWriter}
import java.io.File
import de.fosd.typechef.crefactor.evaluation.util.StopClock

/**
 * Interface for generating linking informations of a whole given project
 */
trait CLinkingInterfaceGenerator extends Evaluation with App {

    val filename = "CLinking"
    val linkExt = ".interface"
    val dbgLinkExt = ".dbginterface"

    val fileList = io.Source.fromFile(filesToEval).getLines().toList
    println(fileList.size + " files to analyse for linking informations.")

    private def getFMConstraints: Iterator[FeatureExpr] =
        for (l: String <- io.Source.fromFile(featureModel).getLines(); if l != "")
        yield new FeatureExprParser().parse(l)

    val fm_const_genClock = new StopClock
    val fm_constraints = getFMConstraints.fold(FeatureExprFactory.True)(_ and _)
    println("Loaded constraints in " + fm_const_genClock.getTime + "ms.")

    val fm_genClock = new StopClock
    //val fm = FeatureExprFactory.default.featureModelFactory.create(fm_constraints)
    val fm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFile(featureModel_DIMACS, "")
    println("Loaded feature model in " + fm_genClock.getTime + "ms.")

    val reader = new InterfaceWriter() {}
    val interfaces = fileList.map(f => reader.readInterface(new File(sourcePath + f + ".interface"))).map(SystemLinker.linkStdLib)

    def linkTreewise(l: List[CInterface]): CInterface = {
        if (l.size > 2) {
            val m: Int = l.size / 2
            val left = l.take(m)
            val right = l.drop(m)
            linkTreewise(List(linkTreewise(left), linkTreewise(right)))
        } else if (l.size == 2) {
            val left = l(0)
            val right = l(1)
            val conflicts = left getConflicts right

            for (c <- conflicts)
                if (!c._2.isTautology(fm))
                    println("Waring: " + c + " is not a tautology in feature model.")

            if (!(left isCompatibleTo right)) {
                println("Conflict: " + conflicts + " is not compatible with feature model.")
                left
            } else left link right
        } else if (l.size == 1) l(0)
        else {
            assert(false, l)
            EmptyInterface
        }
    }

    /** def linkIncrementally(l: List[CInterface]): CInterface = l.fold(EmptyInterface)((left, right) => {
        if (!(left isCompatibleTo right)) {
            println("Conflict: " + (left getConflicts right))
            left
        }
        else left link right
    }) */

    val linkingClock = new StopClock
    val finalInterface = linkTreewise(interfaces).pack //.andFM(fm_constraints)
    println("Linked interfaces in " + linkingClock.getTime + "ms.")

    println("Linked interface is complete:\t" + finalInterface.isComplete)
    println("Linked interface is fully configured:\t" + finalInterface.isFullyConfigured)
    println("Linked interface is wellformed:\t" + finalInterface.isWellformed)

    println(finalInterface.exports.size + " exports: " + finalInterface.exports)
    println(finalInterface.imports.size + " imports: " + finalInterface.imports)

    val interfacePath = new File(completePath + "/" + filename + linkExt)
    val dbgInterfacePath = new File(completePath + "/" + filename + dbgLinkExt)

    reader.writeInterface(finalInterface, interfacePath)
    println("Generated interface in " + interfacePath.getCanonicalPath)

    reader.debugInterface(finalInterface, dbgInterfacePath)
    println("Generated debug interface in " + dbgInterfacePath.getCanonicalPath)

    new CLinking(interfacePath.getCanonicalPath)
}
