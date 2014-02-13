package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import java.util.{Collections, Observable}

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.StatsJar

class Morpheus(tunit: TranslationUnit, fm: FeatureModel, file: String) extends Observable
with CDeclUse with CTypeEnv with CEnvCache with CTypeCache with CTypeSystem with Logging {

    def this(tunit: TranslationUnit) = this(tunit, null, null)
    def this(tunit: TranslationUnit, fm: FeatureModel) = this(tunit, fm, null)
    def this(tunit: TranslationUnit, file: String) = this(tunit, null, file)

    private var tunitCached: TranslationUnit = tunit
    private var astEnvCached: ASTEnv = CASTEnv.createASTEnv(tunit)
    private val typeCheck = new StopClock

    typecheckTranslationUnit(tunit.asInstanceOf[TranslationUnit])
    if (file != null)
        StatsJar.addStat(file, TypeCheck, typeCheck.getTime)

    private val connectedIds: java.util.IdentityHashMap[Id, List[Id]] = new java.util.IdentityHashMap()

    // determines linkage information between identifier uses and declares and vice versa
    //
    // decl-use information in typesystem are determined without the feature model
    // solely on the basis of annotations in the source code
    def linkage(id: Id): List[Id] = {

        val fExpId = astEnvCached.featureExpr(id)

        def isAlreadyConnected(map: IdentityIdHashMap): Boolean = {
            if (!map.containsKey(id))
                return false

            val existingConnectedList = map.get(id).filter(connectedIds.containsKey)
            val connected = !existingConnectedList.isEmpty
            if (connected)
                connectedIds.put(id, connectedIds.get(existingConnectedList.head))

            connected
        }

        if (connectedIds.containsKey(id)
            || isAlreadyConnected(getUseDeclMap)      // use -> decl
            || isAlreadyConnected(getDeclUseMap))     // decl -> use
            return connectedIds.get(id)

        val visited = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())

        // we connect conn only if the feature
        def addToConnectedIdMap(conn: Id) = {

            visited.add(conn)
            val fExpConn = astEnvCached.featureExpr(conn)

            if (fExpId and fExpConn isSatisfiable fm) {
                if (connectedIds.containsKey(id)) connectedIds.put(id, conn :: connectedIds.get(id))
                else connectedIds.put(id, List(conn))
            } else {
                connectedIds.get(id)
            }

        }

        // find all uses of an callId
        def addOccurrence(curId: Id) {
            if (!visited.contains(curId)) {
                addToConnectedIdMap(curId)
                if (getDeclUseMap.containsKey(curId))
                    return getDeclUseMap.get(curId).foreach(use => {
                        addToConnectedIdMap(use)
                        if (getUseDeclMap.containsKey(use))
                            getUseDeclMap.get(use).foreach(entry => addOccurrence(entry))
                    })
            }
        }

        if (getUseDeclMap.containsKey(id))
            getUseDeclMap.get(id).foreach(addOccurrence)
        else
            addOccurrence(id)

        connectedIds.get(id)
    }

    //private var ts = new CTypeSystemFrontend(tunit.asInstanceOf[TranslationUnit], fm)
    //ts.checkAST
    def update(tunit: TranslationUnit) {
        tunitCached = tunit
        astEnvCached = CASTEnv.createASTEnv(tunitCached)
        //ts = new CTypeSystemFrontend(astCached.asInstanceOf[TranslationUnit], fm)
        //ts.checkAST
        typecheckTranslationUnit(tunit)
        setChanged()
        notifyObservers()
    }

    def getEnv(ast: AST) = lookupEnv(ast)

    def getTranslationUnit = tunitCached

    def getASTEnv = astEnvCached

    def getFM = fm

    def getFile = file
}