package de.fosd.typechef.crefactor.evaluation.setup

import de.fosd.typechef.crefactor.evaluation.Evaluation
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}
import de.fosd.typechef.typesystem.linker.{EmptyInterface, CInterface, SystemLinker, InterfaceWriter}
import java.io.File
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.Logging
import de.fosd.typechef.crefactor.backend.CModuleInterface

/**
 * Interface for generating linking informations of a whole given project
 */
trait CModuleInterfaceGenerator extends Evaluation with App with Logging {

    val filename = "CLinking"
    val linkExt = ".interface"
    val dbgLinkExt = ".dbginterface"

    val fileList = io.Source.fromFile(filesToEval).getLines().toList
    logger.info(fileList.size + " files to analyse for linking information.")

    private def getFMConstraints: Iterator[FeatureExpr] =
        for (l: String <- io.Source.fromFile(featureModel).getLines(); if l != "")
        yield new FeatureExprParser().parse(l)

    val fm_const_genClock = new StopClock
    val fm_constraints = getFMConstraints.fold(FeatureExprFactory.True)(_ and _)
    logger.info("Loaded constraints in " + fm_const_genClock.getTime + "ms.")

    val fm_genClock = new StopClock
    //val fm = FeatureExprFactory.default.featureModelFactory.create(fm_constraints)
    val fm = FeatureExprFactory.default.featureModelFactory.createFromDimacsFile(featureModel_DIMACS, "")
    logger.info("Loaded feature model in " + fm_genClock.getTime + "ms.")

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
                    logger.warn(c + " is not a tautology in feature model.")

            if (!(left isCompatibleTo right)) {
                logger.error(conflicts + " is not compatible with feature model.")
                left
            } else left linkWithOutElimination right
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
    val finalInterface = linkTreewise(interfaces)//.packWithOutElimination //.andFM(fm_constraints)
    logger.info("Linked interfaces in " + linkingClock.getTime + "ms.")

    logger.info("Linked interface is complete:\t" + finalInterface.isComplete)
    logger.info("Linked interface is fully configured:\t" + finalInterface.isFullyConfigured)
    logger.info("Linked interface is well-formed:\t" + finalInterface.isWellformed)

    logger.info(finalInterface.exports.size + " exports: " + finalInterface.exports)
    logger.info(finalInterface.imports.size + " imports: " + finalInterface.imports)

    val interfacePath = new File(completePath + "/" + filename + linkExt)
    val dbgInterfacePath = new File(completePath + "/" + filename + dbgLinkExt)

    reader.writeInterface(finalInterface, interfacePath)
    logger.info("Generated interface in " + interfacePath.getCanonicalPath)

    reader.debugInterface(finalInterface, dbgInterfacePath)
    logger.info("Generated debug interface in " + dbgInterfacePath.getCanonicalPath)

    new CModuleInterface(interfacePath.getCanonicalPath)
}
