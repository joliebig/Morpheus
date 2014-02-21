package de.fosd.typechef.crefactor.backend

import java.io.File
import de.fosd.typechef.typesystem.linker.{CSignature, InterfaceWriter}
import java.util
import de.fosd.typechef.error.Position
import scala.collection.parallel.mutable
import de.fosd.typechef.crefactor.Logging
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureModel

class CModuleInterface(linkPath: String) extends Logging {

    val reader = new InterfaceWriter {}
    val linkFile = new File(linkPath)
    val interface = {
        if (linkFile.exists()) reader.loadInterfaceFromXML(xml.XML.loadFile(linkFile))
        else null
    }
    var blackList = new mutable.ParHashSet[String]()

    val idLinkExpMap: util.HashMap[String, List[CSignature]] = new util.HashMap()
    val idLinkPosMap: util.HashMap[String, List[Position]] = new util.HashMap()

    if (interface != null) {
        interface.exports.foreach(addToMaps)
        interface.imports.foreach(expr =>
            if (nameIsListed(expr.name)) addToMaps(expr)
            else blackList += expr.name
        )
    } else logger.info("No linking interface loaded!")

    private def addToMaps(exp: CSignature): List[Position] = {
        addToExpMap(exp.name, exp)
        addToPosMap(exp.name, exp.pos.toList)
    }

    private def addToExpMap(key: String, value: CSignature) =
        if (idLinkExpMap.containsKey(key)) idLinkExpMap.put(key, value :: idLinkExpMap.get(key))
        else idLinkExpMap.put(key, List(value))

    private def addToPosMap(key: String, value: List[Position]) =
        if (idLinkPosMap.containsKey(key)) idLinkPosMap.put(key, value ::: idLinkPosMap.get(key))
        else idLinkPosMap.put(key, value)

    private def nameIsListed(name: String) = idLinkExpMap.containsKey(name) || idLinkPosMap.containsKey(name)

    def isListed(id: Opt[String], fm: FeatureModel): Boolean = {
        if (idLinkExpMap.containsKey(id.entry)) {
            logger.info(id.entry + "is listed with")
            idLinkExpMap.get(id.entry).foreach(logger.info)
            logger.info("incoming feature was: " + id.feature)
            idLinkExpMap.get(id.entry).foreach(sig => logger.info("Is tautology: " + sig.fexpr.and(id.feature).isTautology(fm)))
            idLinkExpMap.get(id.entry).foreach(sig => logger.info("Is satisfiable: " + sig.fexpr.and(id.feature).isSatisfiable(fm)))
            idLinkExpMap.get(id.entry).exists(sig => sig.fexpr.and(id.feature).isTautology(fm))
        } else false
    }

    def isBlackListed(id: String) = blackList.contains(id)

    def getSignatures(id: String) = idLinkExpMap.get(id)

    def getPositions(id: String) = {
        val result = idLinkPosMap.get(id)
        if (result != null) result
        else List[Position]()
    }
}
