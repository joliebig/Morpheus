package de.fosd.typechef.crefactor.evaluation.setup

import java.io.File
import de.fosd.typechef.typesystem.linker.{CSignature, InterfaceWriter}
import java.util
import de.fosd.typechef.error.Position
import scala.collection.parallel.mutable
import de.fosd.typechef.crefactor.Logging

class CLinking(linkPath: String) extends Logging {

    val reader = new InterfaceWriter {}
    val linkFile = new File(linkPath)
    val interface = {
        if (linkFile.exists()) reader.loadInterfaceFromXML(xml.XML.loadFile(linkFile))
        else null
    }
    var blackList = new mutable.ParHashSet[String]()

    val idLinkExpMap: util.IdentityHashMap[String, List[CSignature]] = new util.IdentityHashMap()
    val idLinkPosMap: util.IdentityHashMap[String, List[Position]] = new util.IdentityHashMap()

    if (interface != null) {
        interface.exports.foreach(addToMaps)
        interface.imports.foreach(expr =>
            if (isListed(expr.name)) addToMaps(expr)
            else blackList += expr.name
        )
    } else logger.info("No linking interface loaded!")

    def addToMaps(exp: CSignature): List[Position] = {
        addToExpMap(exp.name, exp)
        addToPosMap(exp.name, exp.pos.toList)
    }

    def addToExpMap(key: String, value: CSignature) =
        if (idLinkExpMap.containsKey(key)) idLinkExpMap.put(key, value :: idLinkExpMap.get(key))
        else idLinkExpMap.put(key, List(value))

    def addToPosMap(key: String, value: List[Position]) =
        if (idLinkPosMap.containsKey(key)) idLinkPosMap.put(key, value ::: idLinkPosMap.get(key))
        else idLinkPosMap.put(key, value)

    def isListed(id: String) = idLinkExpMap.containsKey(id) || idLinkPosMap.containsKey(id)

    def isBlackListed(id: String) = blackList.contains(id)

    def getSignatures(id: String) = idLinkExpMap.get(id)

    def getPositions(id: String) = {
        val result = idLinkPosMap.get(id)
        if (result != null) result
        else List[Position]()
    }
}
