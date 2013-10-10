package de.fosd.typechef.crefactor.evaluation.busybox_1_18_5.linking

import java.io.File
import de.fosd.typechef.typesystem.linker.{CSignature, InterfaceWriter}
import java.util
import de.fosd.typechef.error.Position
import scala.collection.parallel.mutable

class CLinking(linkFile: String) {

    val reader = new InterfaceWriter {}
    val interface = reader.interfaceFromXML(xml.XML.loadFile(new File(linkFile)))
    var blackList = new mutable.ParHashSet[String]()

    // TODO Optimize Data Structure -> Scala Mutable Maps
    val idLinkExpMap: util.IdentityHashMap[String, List[CSignature]] = new util.IdentityHashMap()
    val idLinkPosMap: util.IdentityHashMap[String, List[Position]] = new util.IdentityHashMap()

    interface.exports.foreach(addToMaps)
    interface.imports.foreach(expr =>
        if (isListed(expr.name)) addToMaps(expr)
        else blackList += expr.name
    )

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

    def isListed(id: String) = (idLinkExpMap.containsKey(id) || idLinkPosMap.containsKey(id))

    def isBlackListed(id: String) = blackList.contains(id)

    def getSignatures(id: String) = idLinkExpMap.get(id)

    def getPositions(id: String) = idLinkPosMap.get(id)
}
