#!/usr/bin/tchsh
# mpmt1.tcl: A simple multi-threading example in Tcl
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2024/12/03 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# TODO:
#   * Consider Thread Pool
#   * Implement multi-process mode using fork
#   * Use getopt
#
package require Thread

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

if { $::argc >= 1} {
    set num_context [lindex $::argv 0]
}
if { $::argc >= 2} {
    set duration [lindex $::argv 1]
}
puts "num_context: $num_context duration: $duration"

set tlist {}

for {set idx 0} {$idx < $num_context} {incr idx} {
    set tid [thread::create -joinable]
    #puts "tid: $tid"
    thread::send -async $tid "set duration $duration" result
    thread::send -async $tid $busy_loop result
    lappend tlist $tid
}

foreach t $tlist {
    #puts "calling join to tid: $t"
    thread::join $t
}
#puts "main: exiting.."
