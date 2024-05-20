# mpmt : Multi-Process Multi-Thread examples

This directory contains various (normally) small example programs to see
behaviour differences of multi-process and multi-thread programming model
frameworks of various languages/runtimes.

## 1. A simple busy loop workload generator

* Usage

Common across languages.

```
  $ PROGRAM [-n NUM_CONTEXT] [-d DURATION] [-m MODE]
```

* NUM_CONTEXT
  * number of execution contexts (thread, process or so) default: 4
* DURATION
  * number (in seconds) default: 5 (seconds)
  * duration to generate CPU load
* MODE
  * t or T : thread model (default)
    * In case of golang, 'g' (goroutine).
  * p or P : process model

* Languages
  * Python3: mpmt1.py
    * Uses python threading and multiprocessing.
  * C: mpmt1.c
    * Uses pthread and fork().
    * Build
      * Simplly `make c`, and execute `./mpmt1c`
  * Go: mpmt1.go
      * Uses goroutine. No process model at the moment.
      * Simply `go run mpmt1.go` or`make go`, and `./mpmt1go`
  * Rust: mpmt1.rs
      * Implements thread model only. No process model at the moment.
      * Use nightly tool chain as this uses 'crate'.
      * Simply `make rust `, and `./mpmt1rs`
  * Ruby: mpmt1.rb
      * Implements thread model only. No process model at the moment.
  * Node.js: mpmt1.js
      * Implements thread model only. No process model at the moment.
      * Install 'posix-getopt'
  * Scala: mpmt1.scala
      * Implements thread model only. No process model at the moment.
      * In case of Ubuntu, use scala3.
      * Simply `make scala `, and `scala mpmt1`
  * Lua: mpmt1.lua
      * Uses coroutine of Lua. No multi thread nor process at the moment.
      * To be updated to use getopt.
  * Common Lisp(sbcl): mpmt1.lisp
      * Implements thread model only using 'bordeaux-threads'. No multi process at the moment.
      * Runs under sbcl. Use quicklisp to install 'bordeaux-threads' and 'getopt'
          *  https://www.quicklisp.org/beta/index.html
      * Still buggy...
      * Looks like there are misunderstandings regarding pass-by-value or pass-by-reference in SBCL/Common Lisp. Please see the comment in 'bt:make-thread' block. If we execute it without the '(sleep 1)', busy_worker() prints wrong 'id'.
  * Julia: mpmt1.jl
      * Thread mode only at the moment.
      * Note that you need to increase maximum thread number of Julia runtime by JULIA_NUM_THREADS environment variable not only -n NUM_CONTEXT.
  * Perl: mpmt1.pl
      * Thread mode only at the moment.
      * Note: Perl interpreter-based thread runs parallelly not only concurrently different from Python, Ruby, etc.
  * Elixir: mpmt1.exs
      * Just worked version. Need blush up.
  * Haskell: mpmt1.hs
      * Thread mode (using forkIO) only at the moment.
      * Does not implement getopt yet. Current usage is below:
          * `$ ghc -threaded  -rtsopts mpmt1.hs -o mpmt1hs`
          * `$ ./mpmt1hs DURATION NUM_CONTEXT +RTS -Nn `
          * DURATION is in seconds, n of -N is number of platorm threads)
  * Erlang: mpmt1.erl
      * Similar status with Haskell
      * See comments in mpmt1.erl
  * Java: java/
      * `$ cd java && mvn clean package`
      * Supports Virtual Thread after JDK 19.
      * See java/README.md for the details.

* Notes
  * Run the executables on a server at least 2 (v)CPUs.
  * Observe CPU usage by using top command for example. (Better to refresh frequently using '-d 1'.)
  * You would see python threading model can consume only 1 CPU (100%) even if there are multiple CPUs and you specifed more than 2 contexts.
  * Notes on Python GIL(May 6, 2024)
      * Python community made GIL optional as of 3.13.
          * https://peps.python.org/pep-0703/
      * This enabled mpmt1.py thread mode running in the same parallelism with multiprocessing mode. I checked using in-house built Python 3.13.0a6 on Ubuntu 22.04 with PYTHON_GIL=0 environment variable.

* TODO
  * Add some more languages. (Pascal, C++, C#, Swift, WebAssembly(?), TypeScript, etc.)

## 2. A simple test program for inter thread/process communication

Measures inter thread/process communication performance.

* Languages
  * Python3: mpmt2.py
      * A variation of mpmt1.py You can see difference of queue performance between threading and multiprocessing.

* TODO
  * Implement variable message size measurement.
