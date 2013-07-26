#!/bin/bash
make clean
timeout 600 make -j 8

RETVAL=$?
[ $RETVAL -eq 0 ] && echo "Success_Build"
[ $RETVAL -eq 124 ] && echo "Success_Timeout"
[ $RETVAL -ne 0 ] && echo "Failure_Build"

exit