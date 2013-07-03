package de.fosd.typechef.crefactor.BusyBoxEvaluation

import org.junit.Test
import java.io.File
import de.fosd.typechef.parser.c.{Id, AST}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.crefactor.util.{PrepareRefactoredASTforEval, TimeMeasurement}
import de.fosd.typechef.crefactor.Morpheus
import de.fosd.typechef.crefactor.backend.refactor.RenameIdentifier

class RenameEvaluation extends BusyBoxEvaluation {

    private val REFACTOR_NAME = "refactoredID"


    @Test
    def evaluate() {
        val files = getBusyBoxFiles.reverse
        val refactor = files.map(file => {
            val bb_file = new File(busyBoxPath + file)
            try {
                var stats = List[Any]()
                val parseTypeCheckMs = new TimeMeasurement
                val parsed = parse(bb_file)
                val ast = parsed._1
                val fm = parsed._2
                val morpheus = new Morpheus(ast, fm)
                val parseTypeCheckTime = parseTypeCheckMs.getTime
                stats ::= parseTypeCheckTime
                val result = applyRefactor(morpheus, stats)
                if (result._2) PrepareRefactoredASTforEval.makeConfigs(result._1, morpheus.getFeatureModel, bb_file.getCanonicalPath, result._3, 0)

                val verify = Verification.verify(bb_file, 0, fm)
                var stat2 = result._4
                stat2 = stat2.::(result._2 + "\n" + verify)
                writeStats(stat2, bb_file.getCanonicalPath, 0)
                verify
            } catch {
                case e: Exception => {
                    println(e.getMessage)
                    println(e.getStackTrace.mkString("\n"))
                    writeExeception(e.getMessage + "\n" + e.getStackTrace.mkString("\n"), bb_file.getCanonicalPath, 0)
                    false
                }
            }
        })
        println("Refactor succ: " + refactor.contains(false))
        refactor.contains(false)
    }

    def applyRefactor(morpheus: Morpheus, stat: List[Any]): (AST, Boolean, List[FeatureExpr], List[Any]) = {

        def getVariableIdToRename: (Id, Int, List[FeatureExpr]) = {
            val ids = morpheus.getUseDeclMap.values().toArray(Array[List[Id]]()).par.foldLeft(List[Id]())((list, entry) => list ::: entry)

            val writeableIds = ids.par.filter(id =>
                RenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap).forall(i => new File(i.getFile.get.replaceFirst("file ", "")).canWrite)
            )

            val variabaleIds = writeableIds.par.filter(id => {
                val associatedIds = RenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
                val features = associatedIds.map(x => morpheus.getASTEnv.featureExpr(x))

                if (id.name.equals("main")) false
                else !(features.distinct.length == 1 && features.contains("True"))
            })

            var id: Id = null

            if (!variabaleIds.isEmpty) id = variabaleIds.apply((math.random * variabaleIds.size).toInt)
            else id = writeableIds.apply((math.random * writeableIds.size).toInt)

            val associatedIds = RenameIdentifier.getAllConnectedIdentifier(id, morpheus.getDeclUseMap, morpheus.getUseDeclMap)
            (id, associatedIds.length, associatedIds.map(morpheus.getASTEnv.featureExpr(_)).distinct)
        }

        val toRename = getVariableIdToRename
        val id = toRename._1
        val features = toRename._3

        val startRenaming = new TimeMeasurement
        val refactored = RenameIdentifier.rename(id, REFACTOR_NAME, morpheus)
        val renamingTime = startRenaming.getTime
        var stats = stat.::(renamingTime)
        stats = stats.::(id)
        stats = stats.::(toRename._2)
        stats = stats.::(features)

        val morpheus_ref = new Morpheus(refactored, morpheus.getFeatureModel)

        val originAmount = analsyeDeclUse(morpheus.getDeclUseMap).sorted
        val newAmount = analsyeDeclUse(morpheus_ref.getDeclUseMap).sorted

        (refactored, originAmount == newAmount, features, stats)
    }
}
