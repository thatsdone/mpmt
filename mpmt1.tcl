#!/usr/bin/tchsh
# mpmt1.tcl: A simple multi-threading example in Tcl
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2024/12/03 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# Dependency:
#   * Tclx
# TODO:
#   * Consider Thread Pool
#   * Use getopt
#
package require Thread
package require Tclx

set busy_loop {
    #puts "duration: $duration"
    set ts_start [clock milliseconds]
    #puts "ts_start: $ts_start"
    while {1} {
        set ts_now [clock milliseconds]
        if {$ts_now - $ts_start > $duration * 1000} {
            puts "Expired..."
            thread::exit
        }
    }
}


set num_context 3
set duration 5
set mode "t"

if { $::argc >= 1} {
    set num_context [lindex $::argv 0]
}
if { $::argc >= 2} {
    set duration [lindex $::argv 1]
}
if { $::argc >= 3} {
    set mode [lindex $::argv 2]
}

puts "num_context: $num_context duration: $duration: mode: $mode"

set clist {}

if {$mode == "t"} {
    for {set idx 0} {$idx < $num_context} {incr idx} {
        set tid [thread::create -joinable]
        #puts "tid: $tid"
        thread::send -async $tid "set duration $duration"
        thread::send -async $tid $busy_loop
        lappend clist $tid
    }
    foreach t $clist {
        #puts "calling join to tid: $t"
        thread::join $t
    }

} elseif {$mode == "p"} {
    for {set idx 0} {$idx < $num_context} {incr idx} {
        set pid [fork]
        switch $pid {
            -1 {
                puts "fork() failed. idx: $idx"
                exit
            }
            # child
            0 {
                set ts_start [clock milliseconds]
                #puts "ts_start: $ts_start"
                while {1} {
                    set ts_now [clock milliseconds]
                    if {$ts_now - $ts_start > $duration * 1000} {
                        puts "Expired..."
                        exit
                    }
                }
            }
            # parent
            default {
                lappend clist $pid
                #puts "parent: $idx $pid"
            }
        }
    }

    foreach pid $clist {
        wait $pid
    }

} else {
    puts "Unknown mode: $mode"
}
