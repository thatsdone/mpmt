/*
 * mpmt1.d: A simple example of D Language threading.
 *
 * License:
 *   Apache License, Version 2.0
 * History:
 *   2024/12/03 v0.1 Initial version
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 * BUILD:
 *   * `$ dmd  -of=mpmt1d  mpmt1.d`
 * TODO:
 *   * Add multi-process
 *   * Consider Tasks, Fiber,...
 */
import std.stdio;
import std.datetime;
import std.conv;
//import std.parallelism;
import std.concurrency;
import core.thread;

long get_ts()
{
    Duration ut = Clock.currTime() - SysTime(DateTime(1970, 1, 1), UTC());
    return ut.total!"msecs";
    
}
  
void busy_loop(long duration_ms)
    in (duration_ms > 0)
{
    long ts_start = get_ts();
    while (true) {
        long ts_now = get_ts();
        if ((ts_now - ts_start) > duration_ms) {
            writeln("Exiting...");
            break;
        }
    }
}

void main(string[] argv)
{
    int num_context = 3;
    long duration = 5;

    if (argv.length > 1) {
        num_context = to!int(argv[1]);
    }
    if (argv.length > 2) {
        duration = to!long(argv[2]);
    }
    
    writeln("num_context: ", num_context, " duration: ", duration);

    for (int idx = 0; idx < num_context; idx++) {
        spawn(&busy_loop, duration * 1000);
    }
    thread_joinAll();
}
