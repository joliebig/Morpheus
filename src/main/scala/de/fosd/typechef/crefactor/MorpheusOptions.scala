package de.fosd.typechef.crefactor

import gnu.getopt.{LongOpt, Getopt}
import java.util
import de.fosd.typechef.options.Options.OptionGroup
import de.fosd.typechef.options.{FrontendOptionsWithConfigFiles, RefactorType, Options, FrontendOptions}

class MorpheusOptions extends FrontendOptionsWithConfigFiles {
    private final val F_REFEVAL: Char = Options.genOptionId
    private final val F_REFLINk: Char = Options.genOptionId
    private final val F_CANBUILD: Char = Options.genOptionId
    private final val F_REFSTUDY: Char = Options.genOptionId
    private final val F_PREPAREREF: Char = Options.genOptionId
    private final val F_SHOWGUI: Char = Options.genOptionId
    private final val F_WRITEBUILDCONDITION: Char = Options.genOptionId
    private final val F_PROJECTINTERFACE: Char = Options.genOptionId
    private final val F_PRETTYPRINT: Char = Options.genOptionId

    private var refEvalType: RefactorType = RefactorType.NONE
    private var refStudy: String = ""
    private var linkingInterfaceFile: String = ""

    var refEval: Boolean = false
    var refLink: Boolean = false
    var prepareRef: Boolean = false
    var prettyPrint: Boolean = false
    var canBuild: Boolean = false
    var showGui: Boolean = false
    var writeBuildCondition: Boolean = false
    var writeProjectInterface: Boolean = false

    protected override def getOptionGroups = {
        val groups = new util.ArrayList[OptionGroup](super.getOptionGroups)

        groups.add(
            new Options.OptionGroup("Morpheus options", 1,
                new Options.Option("refEval", LongOpt.REQUIRED_ARGUMENT, F_REFEVAL, null,
                    "Apply and verify random refactoring"),
                new Options.Option("refPrep", LongOpt.NO_ARGUMENT, F_PREPAREREF, null,
                    "Writes out a .ref file containing all found and possible refactorings for this file"),
                new Options.Option("refLink", LongOpt.REQUIRED_ARGUMENT, F_REFLINk, null,
                    "Apply refactorings also on all linked files."),
                new Options.Option("canBuild", LongOpt.NO_ARGUMENT, F_CANBUILD, null,
                    "Tests the possibility of building the pretty printed File"),
                new Options.Option("study", LongOpt.REQUIRED_ARGUMENT, F_REFSTUDY, null,
                    "Defines the used case-study environment"),
                new Options.Option("writeProjectInterface", LongOpt.NO_ARGUMENT, F_PROJECTINTERFACE, null,
                    "Writes interface for a complete case study."),
                new Options.Option("writeBuildCondition", LongOpt.NO_ARGUMENT, F_WRITEBUILDCONDITION, null,
                    "Writes out a .bc file containing the extracted custom build properties of the analyzed file."),
                new Options.Option("prettyPrint", LongOpt.NO_ARGUMENT, F_PRETTYPRINT, null,
                    "Pretty prints the parsed ast as .pp file."),
                new Options.Option("showGui", LongOpt.NO_ARGUMENT, F_SHOWGUI, null,
                    "Shows the cRefactor GUI")
            ))

        groups
    }

    protected override def interpretOption(c: Int, g: Getopt): Boolean = {
        if (c == F_REFEVAL) {
            parse = true
            typecheck = true
            refEval = true
            var refEvalArg = g.getOptarg.trim()
            if (refEvalArg.equalsIgnoreCase(RefactorType.RENAME.toString)) {
                refEvalType = RefactorType.RENAME
            } else if (refEvalArg.equalsIgnoreCase(RefactorType.EXTRACT.toString)) {
                refEvalType = RefactorType.EXTRACT
            } else if (refEvalArg.equalsIgnoreCase(RefactorType.INLINE.toString)) {
                refEvalType = RefactorType.INLINE
            } else {
                refEvalType = RefactorType.NONE
            }
        } else if (c == F_PREPAREREF) {
            parse = true
            typecheck = true
            prepareRef = true
        } else if (c == F_REFLINk) {
            refLink = true
            checkFileExists(g.getOptarg)
            linkingInterfaceFile = g.getOptarg
        } else if (c == F_PROJECTINTERFACE) {
            writeProjectInterface = true
        } else if (c == F_REFSTUDY) {
            refStudy = g.getOptarg
        } else if (c == F_CANBUILD) {
            parse = true
            canBuild = true
        } else if (c == F_SHOWGUI) {
            parse = true
            showGui = true
        } else if (c == F_WRITEBUILDCONDITION) {
            writeBuildCondition = true
        } else if (c == F_PRETTYPRINT) {
            parse = true
            prettyPrint = true
        }else {
            return super.interpretOption(c, g)
        }

        true
    }

    def getLinkingInterfaceFile: String = linkingInterfaceFile
    def getRefStudy: String = refStudy
    def getPreparedRefactoringsFileName = super.getOutputStem + ".pr"
    def getRefactorType: RefactorType = refEvalType
}
