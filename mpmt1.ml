(*
 * mpmt1.ml: A simple CPU load generator example using OCaml threading
 *
 *  License:
 *    Apache License, Version 2.0
 * History:
 *   2024/06/23 v0.1 Initial version
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 * Usage:
 *   - $ docker run --rm -it -v `pwd`:/work  ocaml/opam:debian-11-ocaml-5.3 /bin/bash
 *   - $ ocaml -I +unix -I +threads mpmt1.ml [-n NUM_CONTEXT] [-d DURATION]
 * TODO:
 * - ...
 *)

#load "unix.cma";;
#load "threads.cma";;

let timestamp () = (Float.mul (Unix.gettimeofday()) 1000.)

let rec busy_worker (current, time_left) =
   (* Printf.printf "current: %f time_left %f\n" current time_left; *)
   if time_left > 0. then
     let now = timestamp() in
     let diff = Float.sub now current in
     (* Printf.printf "now: %f diff: %f\n" now diff; *)
     busy_worker (now, (Float.sub time_left diff))
   else
     Printf.printf "Expired.\n"

let num_context = ref 3;;
let duration = ref 5;;
Arg.parse [
  ("-n", Arg.Int (function i -> num_context := i), "");
  ("-d", Arg.Int (function i -> duration := i), "");
] (function s -> ()) "ERROR";;
     
Printf.printf "num_context: %d duration: %d\n" !num_context !duration;;

let t = timestamp();;

(*
busy_worker (t,  (Float.mul (float_of_int !duration) 1000.));;
0.));;
*)

for i = 1 to !num_context do
  let _ = Thread.create busy_worker (t, (Float.mul (float_of_int !duration) 1000.)) in ()
done;;
(*
 TODO(thatsdone): Hold Thread.create() result and use Thread.join()
*)
Printf.printf "DEBUG: Sleeping... %d seconds\n" !duration;;
Unix.sleep(!duration);;

