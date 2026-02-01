#!/bin/bash
#set -x
# mpmt1.sh: Bash version of mpmt1.py
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2026/02/01 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# NOTE:
#   * CPU consumption is significantly different when calling command.
# TODO:
#   * Use getopt
busy_worker() {

    if [ "${1}" == "" ]; then
        echo Specify duration
        exit
    fi
    #threshold=`expr ${1} \* 1000000`
    threshold=$((${1} * 1000000))
    #start=`date +"%s%6N"`
    start=${EPOCHREALTIME/[^0-9]/}
    while true
    do
        #now=`date +"%s%6N"`
        now=${EPOCHREALTIME/[^0-9]/}
        #diff=`expr ${now} \- ${start}`
        diff=$((${now} - ${start}))
        if [ ${diff} -ge ${threshold} ]; then
            echo Expired. ${diff}
            break
        fi
    done
}

num_context=4
duration=5

if [ "$1" != "" ]; then
    num_context=$1
fi
if [ "$2" != "" ]; then
    duration=$2
fi
echo "num_context: "${num_context}" duration: "${duration}

PID_LIST=""
for i in $(seq 1 ${num_context})
do
    echo Creating worker: ${i}
    busy_worker ${duration} &
    PID_LIST="${PID_LIST} $!"
done

for pid in ${PID_LIST}
do
    wait ${PID}
done
