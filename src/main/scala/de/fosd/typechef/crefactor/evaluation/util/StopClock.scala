package de.fosd.typechef.crefactor.evaluation.util

class StopClock {

    private val nsToMs = 1000000

    private val time = java.lang.management.ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime

    def getTime = (currentTime - time) / nsToMs

    private def currentTime = java.lang.management.ManagementFactory.getThreadMXBean.getCurrentThreadCpuTime
}
