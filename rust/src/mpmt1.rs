// mpmt1.rs: A stupid simple example of Rust
//
//   Very early stage version using std::thread no message passing.
//
// License:
//   Apache License, Version 2.0
// History:
//   * 2021/12/25 v0.1 Initial version.
// Author:
//   Masanori Itoh <masanori.itoh@gmail.com>

#![feature(rustc_private)]
//#![feature(explicit_tail_calls)]
extern crate getopts;
extern crate nix;
use nix::unistd::{fork, ForkResult};
use nix::sys::wait::waitpid;

use getopts::Options;
use std::env;
use std::thread;
use std::time::SystemTime;
use std::convert::TryInto;
use std::process::exit;
use tailcall::tailcall;

fn worker(id: i32, duration: i32) {

    #[tailcall]
    fn busy_loop(ts_start: SystemTime, dur_us: u128) {
        let ts_now = SystemTime::now();
        let diff: u128  = ts_now.duration_since(ts_start).unwrap().as_micros();
        if diff >= dur_us {
            return
        }
        busy_loop(ts_start, dur_us)
        //become busy_loop(ts_start, dur_us)
    }

    let ts_start = SystemTime::now();
    let dur_us: u128 = (duration * 1000 * 1000).try_into().unwrap();
    busy_loop(ts_start, dur_us);
    println!("Expired...: {}", id);
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
        let mut processes: Vec<nix::unistd::Pid> = Vec::new();
        for i in 0..num_context {
            match unsafe{fork()} {
                Ok(ForkResult::Parent {child, ..}) => {
                    println!("parent: created {} th child pid: {}", i, child);
                    processes.push(child);
                }
                Ok(ForkResult::Child) => {
                    worker(i, duration);
                    exit(i)
                },
                Err(_) => println!("fork failed."),
            }
        }
        for pid in processes.iter_mut() {
            waitpid(*pid, None).unwrap();
            println!("parent: waitpid returned: pid: {}", pid);
        }
    }
}
