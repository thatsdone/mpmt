//
// mpmt1.vala: A simple multi-threading example in Vala
//
// License:
//   Apache License, Version 2.0
// History:
//   2024/12/01 v0.1 Initial version
// Author:
//   Masanori Itoh <masanori.itoh@gmail.com>
// BUILD:
//  * `$ valac --pkg posix mpmt1.vala -o mpmt1vala`
// TODO:
//  * Eliminate warnings
//  * Eliminate global variables
//  * Use getopt
//  * Write multi process version.
//
class Mpmt.Mpmt1 : GLib.Object {

   public static int64 duration = 5 * 1000000;

    public static void busy_loop() {
        int64 ts_now = 0;
        int64 ts_start = get_monotonic_time();
        //stdout.printf("Hello thread! %lld %lld\n", duration, ts_start);
        while (true) {
            ts_now = get_monotonic_time();
            if ((ts_now - ts_start) > duration) {
                stdout.printf("Expired... %ld\n", (long)(duration / 1000000));
                return;
            }
        }
    }

    public static int main(string[] args) {

        int num_context = 3;
        List<Thread> tlist = new List<Thread<void>>();

        if (args.length > 1) {
            num_context = int.parse(args[1]);
        }
        if (args.length > 2) {
            duration = int.parse(args[2]) * 1000000;
        }
        stdout.printf("num_context: %d duration: %ld\n",
                         num_context, (long)(duration / 1000000));

        // create threads
        for (int i = 0; i < num_context; i++) {
            var t = new Thread<void> ("child thread", busy_loop);
            tlist.append(t);
        }

        // wait for thread termination
        foreach (unowned Thread<void> e in tlist) {
            e.join();
        }

        return 0;
    }
}
