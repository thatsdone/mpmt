#!/usr/bin/env awk
#
# mpmt1.awk: GNU AWK version of mpmt1.py
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2023/12/03 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# TODO:
#   * Use getopt.
#   * Study if thread model is possible.
#   * Study if high precision timer is available.
#
@load "fork"
BEGIN {

    num_context = 3
    duration = 5
    
    if (ARGC > 1)
        num_context = ARGV[1]
    if (ARGC > 2)
        duration = ARGV[2]

    printf("num_context: %d duration: %d\n", num_context, duration)

    for (i = 0; i < num_context; i++) {
        if ((pid = fork()) == 0) {
            # child
            #printf("hello from child %d\n", i)
            ts_start = systime()
            while (1) {
                ts_now = systime()
                if ((ts_now - ts_start) >= duration) {
                    printf("Expired...\n")
                    exit
                }
            }
        } else {
            # parent
            #printf( "hello from parent: %s %d\n", pid, i)
            pids[i] = pid
        }
    }
    for (i = 0; i < num_context; i++) {
        p = wait()  #pid(pids[i])
        printf("pid %d exit...\n", p)
    }
}


