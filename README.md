# mpmt : Multi-Process Multi-Thread examples

This repository contains small example programs to see
behaviour differences of multi-process and multi-thread programming model
framework of various languages/runtimes.

## 1. A simple busy loop workload generator

mpmt1 series is a simple CPU load generator using multi-thread or multi-process
models in various languages.

Basically, the main routine creates specified number of threads/processes, and
the workers do busy loop just repeating to get timestamp and check if specified
duration have passed.

Run programs on a server at least 2 (v)CPUs,
and observe CPU usage using top command for example.
(Better to refresh frequently using '-d 1'.)
You would see that in some languages such as python threading model
can consume only 1 CPU (100%)
even if there are multiple CPUs and you specifed more than 2 contexts.

Here is the list of languages implemented so far.

1. Python (thread/process)
2. C (thread/process)
3. Golang (goroutine(roughly thread))
4. Rust (thread/process)
5. Ruby (thread)
6. Node.js (process/thread)
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
18. C# (ASP.NET Core) (thread)
19. Dart (thread)
20. C++ (thread)
21. OCaml (thread)
22. Zig (thread)
23. Fortran (thread/OpenMP)
24. Ada (thread)
25. Vala (thread)
26. AWK (process)
27. D (thread)
28. Tcl (thread/process)
29. COBOL (process)
30. PHP (thread/process)
31. Mojo (thread)
32. WASM/C (thread)
33. WASM/Rust (thread)
34. Zephyr/C (kernel thread)
35. Kotlin (thread)

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
  * Uses pthread for thread model and fork() for process model
  * Usage
    * `make c`
    * `./mpmt1c`
* Go: mpmt1.go
    * Uses goroutine. No process mode.
    * Usage
        * `go run mpmt1.go` or
        * `make go && ./mpmt1go`
* Rust: rust/
    * Uses nix crate for process model and std::thread for thread model
    * Usage
        * `cd rust`
        * `cargo run [-- [-m (t|p)] [-n NUM_CONTEXT] [-d DURATION]]`
    * Note
        * nix crate need to enable necessary features explicitly.
            * https://lib.rs/crates/nix/features
* Ruby: mpmt1.rb
    * Implements thread mode only. No process mode at the moment.
* Node.js: mpmt1.js
    * Uses 'cluster' for process model, 'worker_thread' for process model
        * https://nodejs.org/api/cluster.html
        * https://nodejs.org/api/worker_threads.html
    * Tested using docker image tagged: iron-bullseye-slim (v20.14.0)
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
    * Thread mode only at the moment. Just worked version.
    * Install elixir (tested on Ubuntu 22.04 bundled 1.12.2 w/Erlang 24.2.1)
* Haskell: mpmt1.hs
    * Thread mode (using forkIO) only at the moment.
    * Install GHC (tested on Ubuntu 22.04 bundled 8.8.4)
    * Does not implement getopt yet.
    * Usage:
        * `make haskell`
        * `$ ./mpmt1hs [NUM_CONTEXT [DURATION]] +RTS -Nn`
        * DURATION is in seconds, n of -N is number of platorm threads
* Erlang: mpmt1.erl
    * Thread mode only at the moment.
    * Install Erlang/OTP at least 24 (tested on Ubuntu 22.04 bundled 24.2.1)
    * Usage:
      * `$ erlc mpmt1.erl`
      * `$ erl -noshell -pa mpmt1.beam  -s mpmt1 start NUM_CONTEXT DURATION -s init stop`
* Java: java/
    * Thread mode only at the moment.
    * Install at least JDK19 to see Virtual Thread feature.
    * Usage
        *  See java/README.md for the details
* Pascal: mpmt1.pas
    * Thread mode only at the moment.
    * Install FreePascal (tested on Ubuntu 22.04 bundled fpc 3.2.2)
    * Usage
        * `$ fpc -o mpmt1pas mpmt1.pas`
        * `$ ./mpmt1pas [NUM_CONTEXT [DURATION]]`
* Swift: mpmt1.swift
    * Thread mode only at the moment.
    * Tested using Swift docker image swift-5.10-RELEASE
* C# (ASP.NET Core): dotnet/
    * Thread mode only at the moment.
    * Tested using docker image mcr.microsoft.com/dotnet/sdk:8.0
    * Usage
        * `$ cd dotnet`
        * `$ dotnet restore`
        * `$ dotnet build`
    	* `$ bin/Debug/net8.0/mpmt1 [NUM_CONTEXT [DURATION]]`
* Dart: dart/
    * Thread mode only at the moment.
    * Tested using docker image dart:3.4.0-sdk
    * `$ dart pub get`
    * `$ dart mpmt1.dart [NUM_CONTEXT [DURATION]]`
* C++: mpmt1.cpp
    * Thread mode only at the moment.
    * Tested using GNU C++ 11.4.0
    * `$ g++ -o mpmt1cpp mpmt1.cpp`
    * `$ ./mpmt1cpp [NUM_CONTEXT [DURATION]]`
* OCaml: mpmt1.ml
    * Thread mode only at the moment.
    * Very early stage. Need to replace Thread by Domain for parallelism.
    * Under development using docker image:  ocaml/opam:debian-11-ocaml-5.3
* Zig: mpmt1.zig
    * Thread mode only at the moment.
    * `$ zig run mpmt1.zig -- [NUM_CONTEXT [DURATION]]`
    * Written for WASM experiment (TBD) originally.
        * TODO: Create .wasm from mpmt1.zig and run it using wasmtime
* Fortran 2008: mpmt1.f08
    * Thread mode via OpenMP only at the moment.
    * Install libgomp1 (in case of Ubuntu)
    * `$ gfortran mpmt1.f08 -o mpmt1f08 -fopenmp`
    * `$ ./mpmt1f08 [NUM_CONTEXT [DURATION]]`
* Ada: mpmt1.adb
    * Thread mode only at the moment.
    * Install gnat in case of Ubuntu
    * `$ gnatmake mpmt1.adb -o mpmt1adb` or`make ada`
    * `$ ./mpmt1adb [NUM_CONTEXT [DURATION]]`
* Vala: mpmt1.vala
    * Thread mode only at the moment.
    * Install vala-bin (at least) in case of Ubuntu
    * `$ valac --pkg posix mpmt1.vala -o mpmt1vala`
    * `$ ./mpmt1vala [NUM_CONTEXT [DURATION]]`
* AWK: mpmt1.awk
    * Process mode only at the moment.
    * Use gawk in case of Ubuntu
    * `$ awk -f mpmt1.awk [NUM_CONTEXT [DURATION]]`
* D: mpmt1.d
    * Thread mode only at the moment.
    * I used DMD available at: https://dlang.org/download.html
    * `$ rdmd mpmt1.d [NUM_CONTEXT [DURATION]]` or
    * `$ dmd  -of=mpmt1d  mpmt1.d` and `./mpmt1d [NUM_CONTEXT [DURATION]]`
* Tcl: mpmt1.tcl
    * Thread and process mode are available.
    * `$ tclsh mpmt1.tcl [NUM_CONTEXT [DURATION [MODE]]]`
* COBOL: mpmt1.cob
    * Process mode only.
    * I used GNU COBOL on Ubuntu 24.04(gnucobol3)
    * `$ cobc -x -free -o mpmt1cob mpmt1.cob`
    * `$ ./mpmt1cob [NUM_CONTEXT [DURATION]]`
* PHP: mpmt1.php
    * Uses pcntl for process model and pthreads for thread model.
    * Tested using docker image php:7.2.26-zts with pthreads/pcntl addition.
    * See notes in mpmt1.php regarding some issues and reasons of PHP 7.x.
    * `$ php mpmt1.php [NUM_CONTEXT [DURATION [MODE]]]`
* Mojo: mpmt1.mojo
    * Thread mode only.
    * Install mojo following: https://docs.modular.com/mojo/manual/get-started/
    * `$ magic init . --format mojoproject`
    * `$ magic shell`
    * `(mojo) $ mojo mpmt1.mojo [NUM_CONTEXT [DURATION]]`
    * How to pass arguments to thread by parallelize() is under investigation.
* WASM/C: mpmt1-wasi.c
    * Thread mode only based on mpmt1.c
    * Followed article below.
      * https://bytecodealliance.org/articles/wasi-threads
    * Tested using wasi-sdk-25.0-x86_64-linux (release) and in-house built wasmtime 29.0.0 (d5ee2a04d 2024-12-06).
    * Set WASI_SDK_PATH environment variable to point the wasi-sdk path.
    * `$ make wasm`
    * `$ wasmtime -W threads=y -S threads=y ./mpmt1-wasi.wasm [-n NUM_CONTEXT] [-d DURATION]`
* WASM/Rust: mpmt1-rust-wasi.rs
    * Almost the same as the initial version of mpmt1.rs (thread only version)
    * Built following official wasm32-wasip1-threads manual
        * https://doc.rust-lang.org/rustc/platform-support/wasm32-wasip1-threads.html
    * `$ rustc --target wasm32-wasip1-threads mpmt1-rust-wasi.rs`
    * `$ wasmtime  -W threads=y -S threads=y mpmt1-rust-wasi.wasm -n NUM_CONTEXT -d DURATION`
    * Tested using rustc 1.85.0-nightly (33c245b9e 2024-12-10) and wasmtime 29.0.0 (d5ee2a04d 2024-12-06)
    * TODO: Explore how to build using cargo
* Zephyr: zephyr/
    * Zephyr RTOS version in C language
    * Very initial version
* Kotlin: kotlin/
    * Thread mode only, coroutine is TBD
    * Tested using JDK 21 and gradle 9.2.1
    * `$ cd kotlin; ./gradlew run [--args "NUM_CONTEXT DURATION "]`

### TODO
* Add some more languages. (Bend, Bash, PL/I, etc.)
* Update some language implementations (Erlang, etc.) to use message passing mechanism for synchronizing main/worker threads.
* Explore WASM implementation in other languages than C. e.g. Go.
* Explore implementations on Embedded OSes such as NuttX, FreeRTOS etc.
* Explore implementations on AUTOSAR based RTOSes such as TRAMPOLINE.
* Try MISRA-C/C++ compliant version.
* Try [SPARK language](https://en.wikipedia.org/wiki/SPARK_(programming_language))
    
## 2. A simple test program for inter thread/process communication

Measures inter thread/process communication performance.

### Languages
* Python3: mpmt2.py
    * A variation of mpmt1.py You can see difference of queue performance between threading and multiprocessing.

### TODO
* Implement variable message size measurement.
