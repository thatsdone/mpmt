#
# mpmt1.mojo: A simple example of CPU load generator in Mojo
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2024/12/08 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# Usage:
#   * Install mojo following: https://docs.modular.com/mojo/manual/get-started/
#   * `$ magic init . --format mojoproject`
#   * `$ magic shell`
#   * `(mojo) $ mojo mpmt1.mojo [NUM_CONTEXT [DURATION]]`
# TODO:
#   * Explore how we can pass arguments through parallelize()
#
# References:
# * https://mojodojo.dev/mojo-team-answers.html
# * https://docs.modular.com/mojo/stdlib/algorithm/functional/parallelize
# * https://stackoverflow.com/questions/76562547/vectorize-vs-parallelize-in-mojo
# * https://github.com/modularml/mojo/blob/main/examples/mandelbrot.mojo
# * https://github.com/modularml/mojo/issues/1660

from time import now
from sys import argv
from algorithm import parallelize

@parameter
fn busy_loop(dummy : Int) -> None:
    var ts_start : Int64 = now()
    var ts_now : Int64
    var dur : Int64 = duration * 1000000000
    #print("duration: ", duration)
    while 1:
        ts_now = now()
        if (ts_now - ts_start) > dur:
            print("Expired...")
            return

# Use a global variable to pass argument through parallelize() for now.
var duration: Int = 12

fn main() raises:
    var num_context = 3

    if (len(argv()) > 1):
        num_context = atol(argv()[1])
    if (len(argv()) > 2):
        duration = atol(argv()[2])
    
    print("num_context: ", num_context, " duration: ", duration)

#   TODO: Q. How can I pass 'duration' to busy_loop?
    parallelize[busy_loop](num_context)
