package de.fosd.typechef.crefactor.util

import java.io.File
import de.fosd.typechef.featureexpr.FeatureModel

trait Verification {

    def verify(bbFile: File, run: Int, fm: FeatureModel): Boolean

    def runTest: String

    def build: (Boolean, String, String)

}
