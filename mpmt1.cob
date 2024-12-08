*>
*> mpmt1.cob: A simple example of COBOL multiprocess CPU load generator
*>
*> License:
*>   Apache License, Version 2.0
*> History:
*>   * 2024/12/07 v0.1 Initial version
*> Author:
*>   Masanori Itoh <masanori.itoh@gmail.com>
*> Usage:
*>   * Install GNU COBOL. I used gnucobol3 on Ubuntu 24.04
*>   * `$ cobc -x -free -o mpmt1cob mpmt1.cob`
*> TODO:
*>   * Use CBL_GC_WAITPID for child process synchronization.
*>   * Explore high-precision timestamp
*>
identification division.
program-id. mpmt1.

data division.
working-storage section.
    01 num_context pic 9999.
    01 duration pic 9999.
    01 arg pic x(64) value spaces.
    01 pid pic s9(9) binary.
    *>01 wait-status pic s9(9) binary.
    01 cnt pic 9999.

procedure division.
main section.

    *> default values
    move 3 to num_context.
    move 12 to duration.

    *> process command line arguments
    *> https://rosettacode.org/wiki/Command-line_arguments#COBOL
    move 1 to cnt.
    accept arg from argument-value.
    perform until arg = spaces
        if cnt = 1
            compute num_context =  function numval(arg)
            *>display "num_context: " num_context
        end-if
        if cnt = 2
            compute duration =  function numval(arg)
            *>display "duration: " duration
        end-if
        move spaces to arg
        accept arg from argument-value
        add 1 to cnt
    end-perform.

    display "num_context: " num_context " duration: " duration

    *> create workers
    move 0 to cnt.
    perform until cnt >= num_context
        *> NOTE: CBL_GC_FORK is case sensitive.
        call "CBL_GC_FORK" returning pid
        evaluate TRUE
            when pid = zero
                *> child process
                *>display "calling busy_loop"
                call 'busy_loop' using content duration
                stop run

            when pid > zero
                *>parent process
                *>TODO: save pid for later waitpid
                continue

            when pid = -1
                display 'CBL_GC_FORK is not available'
                stop run

            when other
                display 'CBL_GC_FORK returned: ' pid
                stop run
        end-evaluate
        add 1 to cnt
    end-perform.

    *> Wait for process termination by sleeping duration.
    *> Better to use waitpid, but variable length array available?
    display "Waiting for child processes via just sleep."
    call "C$SLEEP" using duration

    stop run.

end program mpmt1.

*>
*> busy_loop subroutine
*>
identification division.
program-id. busy_loop.

data division.
working-storage section.
    01 idx pic 999 value 1.
    01 ts_start usage binary-c-long.
    01 ts_now   usage binary-c-long.
    01 ts_diff  usage binary-c-long value 0.
linkage section.
    01 duration pic 9999.

procedure division using duration.

    call "gettimeofday" using ts_start null.

    perform until ts_diff >= duration
        call "gettimeofday" using ts_now null
        subtract ts_start from ts_now giving ts_diff
    end-perform.
    display "Expired...".
end program busy_loop.
