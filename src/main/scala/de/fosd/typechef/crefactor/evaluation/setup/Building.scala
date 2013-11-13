package de.fosd.typechef.crefactor.evaluation.setup

import de.fosd.typechef.parser.c.AST

trait Building {

    def canBuild(ast: AST, file: String): Boolean

}
