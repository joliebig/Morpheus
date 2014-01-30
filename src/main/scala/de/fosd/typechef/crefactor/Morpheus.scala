package de.fosd.typechef.crefactor

import de.fosd.typechef.crefactor.evaluation.Stats._

import de.fosd.typechef.featureexpr.FeatureModel
import de.fosd.typechef.parser.c._
import de.fosd.typechef.typesystem._
import java.util.Observable
import de.fosd.typechef.parser.c.TranslationUnit
import de.fosd.typechef.crefactor.evaluation.util.StopClock
import de.fosd.typechef.crefactor.evaluation.StatsJar

class Morpheus(ast: AST, fm: FeatureModel, file: String) extends Observable with CDeclUse with CTypeEnv with CEnvCache with CTypeCache with CTypeSystem with Logging {
    def this(ast: AST) = this(ast, null, null)

    def this(ast: AST, fm: FeatureModel) = this(ast, fm, null)

    def this(ast: AST, file: String) = this(ast, null, file)

    private var astCached: AST = ast
    private var astEnvCached: ASTEnv = CASTEnv.createASTEnv(ast)
    val typeCheck = new StopClock
    typecheckTranslationUnit(ast.asInstanceOf[TranslationUnit])
    if (file != null) StatsJar.addStat(file, TypeCheck, typeCheck.getTime)

    //private var ts = new CTypeSystemFrontend(ast.asInstanceOf[TranslationUnit], fm)
    //ts.checkAST
    def update(ast: AST) {
        astCached = ast
        astEnvCached = CASTEnv.createASTEnv(astCached)
        //ts = new CTypeSystemFrontend(astCached.asInstanceOf[TranslationUnit], fm)
        //ts.checkAST
        typecheckTranslationUnit(ast.asInstanceOf[TranslationUnit])
        setChanged()
        notifyObservers()
    }

    def getEnv(ast: AST) = lookupEnv(ast)

    def getAST = astCached

    def getASTEnv = astEnvCached

    def getFeatureModel = fm

    def getFile = file
}