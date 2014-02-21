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

class Morpheus(tunit: TranslationUnit, fm: FeatureModel, moduleInterface: CModuleInterface, file: String)
    extends Observable with CDeclUse with CTypeEnv with CEnvCache with CTypeCache with CTypeSystem with Logging {

    def this(tunit: TranslationUnit, fm: FeatureModel) = this(tunit, fm, null, null)
    def this(tunit: TranslationUnit, fm: FeatureModel, file: String) = this(tunit, fm, null, file)

    private var tunitCached: TranslationUnit = tunit
    private var astEnvCached: ASTEnv = CASTEnv.createASTEnv(tunit)
    private val connectedIds: java.util.IdentityHashMap[Id, List[Opt[Id]]] = new java.util.IdentityHashMap()
    private val enforcedFMDeclUse: java.util.IdentityHashMap[Id, List[Id]] = new java.util.IdentityHashMap()
    private val enforcedFMUseDecl: java.util.IdentityHashMap[Id, List[Id]] = new java.util.IdentityHashMap()

    private val typeCheck = new StopClock

    typecheckTranslationUnit(tunit)
    if (file != null)
        StatsCan.addStat(file, TypeCheck, typeCheck.getTime)

    private val enforce = new StopClock
    enforceFmOnCDeclUse(getFM, getASTEnv)
    logger.info("Enforcing featuremodel on decluse in " + enforce.getTime + "ms.")

    override def getDeclUseMap = IdentityIdHashMap(enforcedFMDeclUse)

    override def getUseDeclMap = IdentityIdHashMap(enforcedFMUseDecl)

    // determines linkage information between identifier uses and declares and vice versa
    //
    // decl-use information in typesystem are determined without the feature model
    // solely on the basis of annotations in the source code
    def getReferences(id: Id): List[Opt[Id]] = {

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
            || isAlreadyConnected(getUseDeclMap) // use -> decl
            || isAlreadyConnected(getDeclUseMap)) // decl -> use
            return connectedIds.get(id)

        val visited = Collections.newSetFromMap[Id](new java.util.IdentityHashMap())

        // we connect conn only if the feature
        def addToConnectedIdMap(conn: Id) = {

            visited.add(conn)
            val fExpConn = astEnvCached.featureExpr(conn)
            if (connectedIds.containsKey(id))
                connectedIds.put(id, Opt(fExpConn, conn) :: connectedIds.get(id))
            else connectedIds.put(id, List(Opt(fExpConn, conn)))

        }

        // find all uses of an callId
        def addOccurrence(curId: Id) {
            if (!visited.contains(curId)) {
                addToConnectedIdMap(curId)

                // TODO: the check is not necessary, as curId is always a declaration
                if (getDeclUseMap.containsKey(curId)) {
                    getDeclUseMap.get(curId).foreach(use => {
                        addToConnectedIdMap(use)

                        // TODO: the check is not necessary either, as Id use should be always
                        //       part of this map
                        if (getUseDeclMap.containsKey(use))
                            getUseDeclMap.get(use).foreach(addOccurrence)
                    })
                }
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
        typecheckTranslationUnit(tunitCached)
        enforceFmOnCDeclUse(getFM, getASTEnv)
        setChanged()
        notifyObservers()
    }

    def getEnv(ast: AST) = lookupEnv(ast)

    def getTranslationUnit = tunitCached

    def getASTEnv = astEnvCached

    def getFM = fm

    def getFile = file

    def getModuleInterface = moduleInterface

    // enforces the feature model on cDeclUse - at removes all invalid connections determined by the feature model
    private def enforceFmOnCDeclUse(featureModel: FeatureModel, astEnv: ASTEnv) = {
        def addToTargetMap(key: Id, entry: Id, targetMap: java.util.IdentityHashMap[Id, List[Id]]) = {
            if (targetMap.containsKey(key)) targetMap.put(key, entry :: targetMap.get(key))
            else targetMap.put(key, List(entry))
        }

        def fillMap(sourceMap: IdentityIdHashMap, targetMap: java.util.IdentityHashMap[Id, List[Id]]) = {
            targetMap.clear()
            sourceMap.keys.foreach(key =>
                sourceMap.get(key).foreach(entry =>
                    if ((astEnv.featureExpr(entry) and astEnv.featureExpr(key)) isSatisfiable (fm))
                        addToTargetMap(key, entry, targetMap)))
        }

        fillMap(super.getDeclUseMap, enforcedFMDeclUse)
        fillMap(super.getUseDeclMap, enforcedFMUseDecl)
    }
}