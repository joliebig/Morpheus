package de.fosd.typechef.crefactor.backend.codeselection

import de.fosd.typechef.parser.c.{ConditionalNavigation, ASTNavigation, Id, AST}
import de.fosd.typechef.crefactor.frontend.util.CodeSelection
import de.fosd.typechef.crefactor.{Logging, Morpheus}
import de.fosd.typechef.conditional.Opt

trait ASTSelection extends Logging with ASTNavigation with ConditionalNavigation {

    def getSelectedElements(morpheus: Morpheus, selection: CodeSelection): List[AST]

    def getAvailableIdentifiers(morpheus: Morpheus, selection: CodeSelection): List[Id]

    def isPartOfSelection(node: AST, selection: CodeSelection): Boolean =
        /**
         * Annotated AST nodes have often the same starting line. As workaround we only identify
         * the element by its end value. Note: not really stable. May cause to not find the appropriated selected
         * statement in same cases.
         */
        isInRange(node.getPositionTo.getLine, selection.getLineStart + 1, selection.getLineEnd - 1)


    /**
     * Compare the position of two AST nodes.
     */
    def comparePosition(e1: AST, e2: AST) = e1.getPositionFrom < e2.getPositionFrom
    def comparePosition(e1: Opt[AST], e2: Opt[AST]): Boolean = comparePosition(e1.entry, e2.entry)

    /**
     * Checks if an AST node (here pos) belongs to a given range [start, end]
     */
    def isInRange(pos: Int, start: Int, end: Int) = (start <= pos) && (pos <= end)

    /**
     * Remove all AST elements except those that belong to the given file.
     */
    def filterASTElementsForFile[T <: AST](selection: List[T], file: String): List[T] =
        selection.filter(p => isPartOfFile(p, file)).toList

    def isPartOfFile[T <: AST](element: T, file: String) =
        // offset 5 because file path of callId contains the string "file "
        element.getFile.get.regionMatches(true, 5, file, 0, file.length())

    // Determine whether a given AST element is part of a code selection in an editor.
    // We use position information, attached to AST nodes by parser, for comparison.
    def isElementOfSelection(element: AST, selection: CodeSelection): Boolean = {
        val startLine = selection.getLineStart
        val endLine   = selection.getLineEnd
        val startRow  = selection.getRowStart
        val endRow    = selection.getRowEnd

        if (!(isInRange(element.getPositionFrom.getLine, startLine, endLine)
            && isInRange(element.getPositionTo.getLine, startLine, endLine)))
            false
        else if (element.getPositionFrom.getLine == startLine)
            isInRange(element.getPositionFrom.getColumn,
                scala.math.min(startRow, endRow),
                scala.math.max(startRow, endRow))
        else if (element.getPositionTo.getLine == endLine)
            isInRange(element.getPositionTo.getColumn,
                scala.math.min(startRow, endRow),
                scala.math.max(startRow, endRow))
        else
            true
    }
}
