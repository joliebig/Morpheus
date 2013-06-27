#!/bin/bash

mainpid=$$
(sleep 600;
echo "Failure Timeout";
kill $mainpid) &
watchdogpid=$!

make -j 8

RETVAL=$?
[ $RETVAL -eq 0 ] && echo "Success"
[ $RETVAL -ne 0 ] && echo "Failure Build"

kill $watchdogpid
