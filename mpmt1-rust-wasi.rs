// mpmt1.rs: 
//
//
// License:
//   Apache License, Version 2.0
// History:
//   * 2021/12/31 v0.1 Initial version.
// Author:
//   Masanori Itoh <masanori.itoh@gmail.com>
// TODO:

#![feature(rustc_private)]
extern crate getopts;

use getopts::Options;
use std::env;
use std::thread;
use std::time::SystemTime;
use std::convert::TryInto;

fn worker(id: i32, duration: i32) {
    let ts_save = SystemTime::now();
    let max: u128 = (duration * 1000 * 1000).try_into().unwrap();

    println!("worker: {} started. duration: {:?} (us)", id, max);

    let mut diff: u128;
    loop {
        let ts = SystemTime::now();
        diff = ts.duration_since(ts_save).unwrap().as_micros();
        if diff >= max {
            break;
        }
    }
    println!("worker: {} exiting... duration: {:?} (us)", id, diff);
}
    

fn main() {

    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optmulti("n", "", "number of contexts", "NUM_CONTEXT");
    opts.optmulti("d", "", "duration", "DURATION");
    opts.optmulti("m", "", "mode", "t(hread)");

    let matches = opts.parse(&args[1..]).unwrap_or_else(|f| panic!("{}", f.to_string()));
    let num_context: i32 =  if matches.opt_present("n") {
        matches.opt_strs("n")[0].parse::<i32>().unwrap()
    } else {
        3
    };
    let duration: i32 = if matches.opt_present("d") {
        matches.opt_strs("d")[0].parse::<i32>().unwrap()
    } else {
        5
    };
    let mode = if matches.opt_present("m") {
        &matches.opt_strs("m")[0]
    } else {
        "t"
    };

    println!("num_context: {}, duration: {}, mode: {}", num_context, duration, mode);

    if mode == "t" {
        let mut handles: Vec<thread::JoinHandle<()>> = Vec::new();
        for i in 0..num_context {
            println!("main: creating {} th thread.", i);
            let thread = thread::spawn(move || {
                worker(i, duration)
            });
            handles.push(thread);
        }
        for thread in handles.into_iter() {
            thread.join().unwrap();
        }

    } else {
        println!("process mode not supported.");
    }
}
