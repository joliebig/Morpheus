package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import java.util.Observable
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.StatsJar

class Morpheus(tunit: TranslationUnit, fm: FeatureModel, file: String) extends Observable with CDeclUse with CTypeEnv with CEnvCache with CTypeCache with CTypeSystem with Logging {
    def this(tunit: TranslationUnit) = this(tunit, null, null)
    def this(tunit: TranslationUnit, fm: FeatureModel) = this(tunit, fm, null)
    def this(tunit: TranslationUnit, file: String) = this(tunit, null, file)

    private var tunitCached: TranslationUnit = tunit
    private var astEnvCached: ASTEnv = CASTEnv.createASTEnv(tunit)
    val typeCheck = new StopClock
    typecheckTranslationUnit(tunit.asInstanceOf[TranslationUnit])
    if (file != null) StatsJar.addStat(file, TypeCheck, typeCheck.getTime)

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

    def getFeatureModel = fm

    def getFile = file
}