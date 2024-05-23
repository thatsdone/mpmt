# mpmt : Multi-Process Multi-Thread examples

This directory contains small example programs to see
behaviour differences of multi-process and multi-thread programming model
frameworks of various languages/runtimes.

## 1. A simple busy loop workload generator

Run executables on a server at least 2 (v)CPUs, and observe CPU usage using top command for example. (Better to refresh frequently using '-d 1'.) You would see that in some languages such as python threading model can consume only 1 CPU (100%) even if there are multiple CPUs and you specifed more than 2 contexts.

Here is the list of languages implemented.

1. Python (thread/process)
2. C (thread/process)
3. Golang (goroutine(roughtly thread))
4. Rust (thread)
5. Ruby (thread)
6. Node.js (process)
7. Scala (thread)
8. Lua (coroutine)
9. Common Lisp (thread)
10. Julia (thread)
11. Perl (thread)
12. Elixir (thread)
13. Haskell (thread)
14. Erlang (thrad)
15. Java (thread)
16. Pascal (thread)
17. Swift (thread)

### Usage

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
  * t or T : thread mode (default)
    * In case of golang, 'g' (goroutine).
  * p or P : process mode

### Languages
* Python3: mpmt1.py
    * Uses python threading and multiprocessing.
    * Notes on Python GIL(May 6, 2024)
        * Python community made GIL optional as of 3.13.
          * https://peps.python.org/pep-0703/
        * This enabled mpmt1.py thread mode running in the same parallelism with multiprocessing mode. I checked using in-house built Python 3.13.0a6 on Ubuntu 22.04 with PYTHON_GIL=0 environment variable.
* C: mpmt1.c
  * Uses pthread and fork().
  * Build
    * Simplly `make c`, and execute `./mpmt1c`
* Go: mpmt1.go
    * Uses goroutine. No process mode at the moment.
    * Simply `go run mpmt1.go` or`make go`, and `./mpmt1go`
* Rust: mpmt1.rs
    * Implements thread mode only. No process mode at the moment.
    * Use nightly tool chain as this uses 'crate'.
    * Simply `make rust `, and `./mpmt1rs`
* Ruby: mpmt1.rb
    * Implements thread mode only. No process mode at the moment.
* Node.js: mpmt1.js
    * Implements process mode only. No thread mode at the moment.
    * Install 'posix-getopt'
* Scala: mpmt1.scala
    * Implements thread mode only. No process mode at the moment.
    * In case of Ubuntu, use scala3.
    * Simply `make scala `, and `scala mpmt1`
* Lua: mpmt1.lua
    * Uses coroutine of Lua. No multi thread nor process at the moment.
* Common Lisp: mpmt1.lisp
    * Implements thread mode only using 'bordeaux-threads'. No multi process at the moment. Still buggy...
    * Install sbcl, and use quicklisp to install 'bordeaux-threads' and 'getopt'
        *  https://www.quicklisp.org/beta/index.html
    * Note
        * Looks like there are some my misunderstandings regarding pass-by-value or pass-by-reference in SBCL/Common Lisp. Please see the comment in 'bt:make-thread' block. If we execute this program without the '(sleep 1)', busy_worker() prints out wrong 'id'.
* Julia: mpmt1.jl
    * Thread mode only at the moment.
    * Note that you need to increase maximum thread number of Julia runtime by JULIA_NUM_THREADS environment variable not only -n NUM_CONTEXT.
* Perl: mpmt1.pl
    * Thread mode only at the moment.
    * Note: Perl interpreter-based thread runs parallelly not only concurrently different from Python, Ruby, etc.
* Elixir: mpmt1.exs
    * Just worked version. Thread mode only at the moment.
* Haskell: mpmt1.hs
    * Thread mode (using forkIO) only at the moment.
    * Install GHC.
    * Does not implement getopt yet. Current usage is below:
        * `$ ghc -threaded  -rtsopts mpmt1.hs -o mpmt1hs`
        * `$ ./mpmt1hs NUM_CONTEXT DURATION +RTS -Nn `
        * DURATION is in seconds, n of -N is number of platorm threads
* Erlang: mpmt1.erl
    * Thread mode only at the moment.
    * Similar status with Haskell
    * See comments in mpmt1.erl
* Java: java/
    * Thread mode only at the moment.
    * Use at least JDK19 to see Virtual Thread feature. See java/README.md for the details.
    * Usage
        * `$ cd java && mvn clean package`
* Pascal: mpmt1.pas
    * Thread mode only at the moment.
    * Install FreePascal.
    * Usage
        * `$ fpc -o mpmt1pas mpmt1.pas`
        * `$ ./mpmt1pas NUM_CONTEXT DURATION`
* Swift: mpmt1.swift
    * Thread mode only at the moment.
    * Tested using Swift docker image swift-5.10-RELEASE

### TODO
* Add some more languages. (C++, C#, WebAssembly(?), TypeScript, etc.)


## 2. A simple test program for inter thread/process communication

Measures inter thread/process communication performance.

### Languages
* Python3: mpmt2.py
    * A variation of mpmt1.py You can see difference of queue performance between threading and multiprocessing.

### TODO
* Implement variable message size measurement.
