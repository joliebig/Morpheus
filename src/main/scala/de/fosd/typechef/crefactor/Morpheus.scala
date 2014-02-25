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
import java.util

class Morpheus(tunit: TranslationUnit, fm: FeatureModel, moduleInterface: CModuleInterface, file: String)
    extends Observable with Logging {

    def this(tunit: TranslationUnit, fm: FeatureModel) = this(tunit, fm, null, null)
    def this(tunit: TranslationUnit, fm: FeatureModel, file: String) = this(tunit, fm, null, file)

    private var tunitCached: TranslationUnit = tunit
    private var astEnvCached: ASTEnv = CASTEnv.createASTEnv(tunit)
    private val ts = new CTypeSystemFrontend(tunit, fm) with CTypeCache with CDeclUse

    private val connectedIdsCache: java.util.IdentityHashMap[Id, List[Opt[Id]]] = new java.util.IdentityHashMap()

    private val typeCheck = new StopClock
    ts.typecheckTranslationUnit(tunit)
    private val typeCheckTime = typeCheck.getTime

    if (file != null)
        StatsCan.addStat(file, TypeCheck, typeCheckTime)

    def getDecls(id: Id): List[Id] = getEnforcedFmEntriesFromMap(id, getTypeSystem.getUseDeclMap)

    def getUses(id: Id): List[Id] = getEnforcedFmEntriesFromMap(id, getTypeSystem.getDeclUseMap)

    def getAllUses : List[Id] = getUseDeclMap.keys

    def isInDeclUseMap(id: Id) = getDeclUseMap.containsKey(id)

    def isInUseDeclMap(id: Id) = getUseDeclMap.containsKey(id)

    // determines linkage information between identifier uses and declares and vice versa
    def getReferences(id: Id): List[Opt[Id]] = {
        if (connectedIdsCache.containsKey(id))
            return connectedIdsCache.get(id)

        val visited = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())

        def addToConnectedIdMap(conn: Id) = {
            visited.add(conn)
            val fExpConn = astEnvCached.featureExpr(conn)
            if (connectedIdsCache.containsKey(id))
                connectedIdsCache.put(id, Opt(fExpConn, conn) :: connectedIdsCache.get(id))
            else connectedIdsCache.put(id, List(Opt(fExpConn, conn)))

        }

        def visitIds(curId: Id) = {
            addToConnectedIdMap(curId)
            getUses(curId).foreach(use => {
                addToConnectedIdMap(use)
                getDecls(use).foreach(addOccurrence)
            })
        }

        // find all uses of an callId
        def addOccurrence(curId: Id) =
            if (!visited.contains(curId))
                visitIds(curId)

        if (isInUseDeclMap(id))
            getDecls(id).foreach(addOccurrence)
        else
            addOccurrence(id)

        connectedIdsCache.get(id)
    }

    //private var ts = new CTypeSystemFrontend(tunit.asInstanceOf[TranslationUnit], fm)
    //ts.checkAST
    def update(tunit: TranslationUnit) {
        tunitCached = tunit
        astEnvCached = CASTEnv.createASTEnv(tunitCached)
        //ts = new CTypeSystemFrontend(astCached.asInstanceOf[TranslationUnit], fm)
        //ts.checkAST
        ts.typecheckTranslationUnit(tunitCached)
        setChanged()
        notifyObservers()
    }

    def getEnv(ast: AST) = ts.lookupEnv(ast)

    def getTranslationUnit = tunitCached

    def getASTEnv = astEnvCached

    def getFM = fm

    def getFile = file

    def getModuleInterface = moduleInterface

    def getTypeSystem = ts

    // TODO make private and replace all references
    def getDeclUseMap = getTypeSystem.getDeclUseMap

    // TODO make private and replace all references
    def getUseDeclMap = getTypeSystem.getUseDeclMap

    // decl-use information in typesystem are determined without the feature model
    // solely on the basis of annotations in the source code
    private def getEnforcedFmEntriesFromMap(key: Id, map: IdentityIdHashMap): List[Id] = {
        if (!map.containsKey(key)) List()
        else map.get(key).flatMap(entry => {
            if (getASTEnv.featureExpr(entry) and getASTEnv.featureExpr(key) isSatisfiable getFM) Some(entry)
            else None
        })
    }

}