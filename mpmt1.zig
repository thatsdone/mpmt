/// mpmt1.zig: A simple example of Zig multi threading
///
/// License:
///   Apache License, Version 2.0
/// History:
///   * 2024/07/08 v0.1 Initial version
/// Author:
///   Masanori Itoh <masanori.itoh@gmail.com>
/// TODO:
///   * Implement process model (if possible)
///   * Use getopt

const std = @import("std");

fn busy_worker(duration: u64) !void {

    std.debug.print("busy_worker_called {d}\n", .{duration});

    const ts_save = std.time.milliTimestamp();

    while (true) {
        const ts = std.time.milliTimestamp();
        if (ts - ts_save > duration) {
            std.debug.print("Expired.\n", .{});
            break;
        }
    }
}

pub fn main() !void {

    var num_context: usize = 3;
    var duration: u64 = 12;

    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    if (args.len >= 2) {
        num_context = try std.fmt.parseInt(usize, args[1], 10);
    }
    if (args.len >= 3) {
        duration = try std.fmt.parseInt(u64, args[2], 10);
    }

    std.debug.print("num_context: {d} duration: {d}\n", .{num_context, duration});

    // Create busy_worker threads
    var tlist = std.ArrayList(std.Thread).init(allocator);
    defer tlist.deinit();
    for (0 .. num_context) |i| {
        //https://github.com/ziglang/zig/blob/master/lib/std/Thread.zig
        std.debug.print("Creating {d} th thread. \n", .{i});
        const th = try std.Thread.spawn(.{}, busy_worker, .{@as(u64, duration * 1000)});
        try tlist.append(th);
    }
    // Join busy_worker threads
    for (tlist.items) |th| {
        std.Thread.join(th);
    }
}
