#!/bin/bash -e
#!/bin/bash -vxe

path=/home/janker/FOSD/evalExtract/cRefactor-BusyBoxEvaluation

filesToProcess() {
  local listFile=busybox/busybox_files
  cat $listFile
}

flags="--refEval extract --refLink $path/busyboxfinal.interface
  -x CONFIG_ \
  --bdd \
  -c $path/redhat.properties \
  --include $path/config.h \
  -I $path/busybox-1.18.5/include \
  --featureModelDimacs $path/BB_fm.dimacs \
  --recordTiming --parserstatistics --lexNoStdout \
  -U HAVE_LIBDMALLOC \
  -DCONFIG_FIND \
  -U CONFIG_FEATURE_WGET_LONG_OPTIONS \
  -U ENABLE_NC_110_COMPAT \
  -U CONFIG_EXTRA_COMPAT \
  -D_GNU_SOURCE"


filesToProcess|while read i; do
         ./cRefactor.sh $path/busybox-1.18.5/$i.c $flags
	 done
