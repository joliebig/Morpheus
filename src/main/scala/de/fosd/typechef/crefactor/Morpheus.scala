package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import java.util.{Collections, Observable}

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.StatsCan
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.crefactor.backend.CModuleInterface

/*
 * Morpheus is the central class that maintains different caches and data structures for refactorings.
 * Among these are the translation unit of the file, binding information, the module interface (binding
 * across files), the feature model, and the ast environment.
 */
class Morpheus(tunit: TranslationUnit, fm: FeatureModel, moduleInterface: CModuleInterface, file: String)
    extends Observable with Logging {

    def this(tunit: TranslationUnit, fm: FeatureModel) = this(tunit, fm, null, null)
    def this(tunit: TranslationUnit, fm: FeatureModel, file: String) = this(tunit, fm, null, file)

    private var tunitCached: TranslationUnit = null
    private var astEnvCached: ASTEnv = null
    private var ts : CTypeSystemFrontend with CTypeCache with CDeclUse = null
    private var tcStatsGen : Boolean = true

    update(tunit)

    def getDecls(id: Id): List[Id] = getSatisfiableReferences(id, getTypeSystem.getUseDeclMap)

    def getUses(id: Id): List[Id] = getSatisfiableReferences(id, getTypeSystem.getDeclUseMap)

    def getAllUses: List[Id] = getUseDeclMap.keys

    def isInDeclUseMap(id: Id) = getDeclUseMap.containsKey(id)

    def isInUseDeclMap(id: Id) = getUseDeclMap.containsKey(id)

    // retrieves all missed but referenced id uses passed in the argument id list
    def getExternalUses(ids: List[Id]) = getExternalVariableReferences(ids, getUses)

    // retrieves all missed but referenced id declarations passed in the argument id list
    def getExternalDefs(ids: List[Id]) = getExternalVariableReferences(ids, getDecls)

    // cache for getReferences
    private val idRefsCache: java.util.IdentityHashMap[Id, List[Opt[Id]]] = new java.util.IdentityHashMap()

    // determines reference information between id uses and id declarations
    // compute transitive closure of identifier uses and declarations starting from id
    def getReferences(id: Id): List[Opt[Id]] = {
        if (idRefsCache.containsKey(id))
            return idRefsCache.get(id)

        val visited = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())

        def markVisited(conn: Id) = {
            visited.add(conn)
            val fExpRef = astEnvCached.featureExpr(conn)
            if (idRefsCache.containsKey(id))
                idRefsCache.put(id, Opt(fExpRef, conn) :: idRefsCache.get(id))
            else
                idRefsCache.put(id, List(Opt(fExpRef, conn)))
        }

        def exporeIdUse(curId: Id) {
            if (!visited.contains(curId)) {
                markVisited(curId)
                getUses(curId).foreach(use => {
                    markVisited(use)
                    getDecls(use).foreach(exporeIdUse)
                })
            }
        }


        if (isInUseDeclMap(id))
            getDecls(id).foreach(exporeIdUse)
        else
            exporeIdUse(id)

        idRefsCache.get(id)
    }

    def update(tunit: TranslationUnit) {
        idRefsCache.clear()
        tunitCached = tunit
        astEnvCached = CASTEnv.createASTEnv(getTranslationUnit)
        ts = new CTypeSystemFrontend(getTranslationUnit, getFM) with CTypeCache with CDeclUse

        val typeCheck = new StopClock
        getTypeSystem.typecheckTranslationUnit(getTranslationUnit)
        val typeCheckTime = typeCheck.getTime

        if (file != null && tcStatsGen) {
            tcStatsGen = false
            StatsCan.addStat(file, TypeCheck, typeCheckTime)
        }
        setChanged()
        notifyObservers()
    }

    def getEnv(ast: AST) = getTypeSystem.lookupEnv(ast)

    def getTranslationUnit = tunitCached

    def getASTEnv = astEnvCached

    def getFM = fm

    def getFile = file //.replace("/Users/andi/Desktop/FOSD", "/local/janker/eval/rename")

    def getModuleInterface = moduleInterface

    def getTypeSystem = ts

    private def getDeclUseMap = getTypeSystem.getDeclUseMap

    private def getUseDeclMap = getTypeSystem.getUseDeclMap

    // reference information (declaration-use) in the typesystem are determined without the feature model
    // solely on the basis of annotations in the source code
    // to rule out references that do not occur in any variant of the system we use the feature model here
    private def getSatisfiableReferences(key: Id, map: IdentityIdHashMap): List[Id] = {
        if (!map.containsKey(key))
            List()
        else
            map.get(key).filter { getASTEnv.featureExpr(_) and getASTEnv.featureExpr(key) isSatisfiable getFM }
    }

    private def getExternalVariableReferences(ids: List[Id], getReferences: (Id) => List[Id]) = {
        ids.flatMap(id => {
            if (!isPartOfFuncCall(id)) {
                val external = getReferences(id).par.flatMap(refId => {
                    if (ids.par.exists(oId => oId.eq(refId))) None
                    else Some(refId)
                }).toList
                if (external.isEmpty) None
                else Some(id, external)
            } else None
        }).toList
    }

    def isPartOfFuncCall(id: Id): Boolean = {
        getASTEnv.parent(id) match {
            case PostfixExpr(`id`, FunctionCall(_)) => true
            case _ => false
        }
    }

}
