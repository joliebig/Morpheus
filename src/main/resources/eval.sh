#!/bin/bash

filesToProcess() {
  local listFile=busybox_files
  cat $listFile
  #awk -F: '$1 ~ /.c$/ {print gensub(/\.c$/, "", "", $1)}' < linux_2.6.33.3_pcs.txt
}

path=local/janker/casestudies/busybox/

filesToProcess|while read i; do
    timeout 900 java -Xmx1024M -Xss256M -XX:PermSize=256M -XX:MaxPermSize=512M -jar sbt-launch.jar "project CRefactor" "run-main de.fosd.typechef.crefactor.CRefactorFrontend $path/busybox-1.18.5/$i.c -c $path/redhat.properties -x CONFIG_ --include $path/config.h -I $path/busybox-1.18.5/include --featureModelFExpr $path/featureModel --interface --debugInterface --recordTiming --lexNoStdout --refEval rename --parserstatistics -U HAVE_LIBDMALLOC -DCONFIG_FIND -U CONFIG_FEATURE_WGET_LONG_OPTIONS -U ENABLE_NC_110_COMPAT -U CONFIG_EXTRA_COMPAT -D_GNU_SOURCE"
done