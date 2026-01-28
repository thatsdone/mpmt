#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# mpmt1.nim: Nim version of mpmit1
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2026/01/28 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# TODO:
#   * Try malebolgia
#
import std/os
import std/times
import std/threadpool
import std/strutils

proc busyWorker(identity: int, duration: int) {. gcsafe .} =
    echo "busyWorker called. ", identity
    let start = getTime()
    let start_ts = start.toUnix() * 1_000_000_000 + start.nanosecond 
    let end_ts = start_ts + duration * 1_000_000_000

    while true:
        let now = getTime()
        let now_ts = now.toUnix() * 1_000_000_000 + now.nanosecond
        if now_ts >= end_ts:
            echo "Expired."
            break

when isMainModule:
    var num_context = 4
    var duration = 5

    let args = commandLineParams()

    if args.len >= 1:
        num_context = parseInt(args[0])
    if args.len >= 2:
        duration = parseInt(args[1])

    echo "num_context: ", num_context, " duration: ", duration

    for i in 0..num_context - 1:
        spawn busyWorker(i, duration)
    sync()
