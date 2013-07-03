#!/bin/bash

mainpid=$$
(sleep 600;
echo "Failure_Timeout";
kill $mainpid) &
watchdogpid=$!

make -j 3

RETVAL=$?
[ $RETVAL -eq 0 ] && echo "Success_Build"
[ $RETVAL -ne 0 ] && echo "Failure_Build"

kill $watchdogpid
