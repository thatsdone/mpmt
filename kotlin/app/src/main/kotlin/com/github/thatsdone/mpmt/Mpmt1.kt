/*
 * Mpmt1.kt : Kotlin version mpmt1
 *
 * License:
 *   Apache License, Version 2.0
 * History:
 *   * 2025/12/20 v0.1 Initial version
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 * TODO:
 *   * Consider Kotlin coroutine
 */
package com.github.thatsdone.mpmt
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.TimeSource
import kotlin.system.measureNanoTime

import kotlin.concurrent.thread

fun main(args: Array<String>) {
    var num_context: Int = 4
    var duration: Int = 5

    val threads = mutableListOf<Thread>()

    for (i in args.indices) {
        if (i == 0) {
            num_context = args[i].toInt()
        }
        if (i == 1) {
            duration = args[i].toInt()
        }
    }
    println("num_context: ${num_context} duration: ${duration}")
    
    repeat(num_context) { i ->
        val t = thread {
            worker(i, duration)
        }
        threads.add(t)
    }
    println("Created ${num_context} threads. Waiting for terminate...")

    threads.forEach {it.join()}

    println("Completed to join ${num_context} threads")
}


fun worker(id: Int, duration: Int) {
    //println("worker called. id: $id duration: $duration")
    val start = TimeSource.Monotonic.markNow()
    var now  = start.elapsedNow()
    val end = duration * 1000 * 1000

    while (true) {
        now = start.elapsedNow()
        if (now.inWholeMicroseconds >= end) {
            println("worker: ${id} Expired")
            return
        }
    }
}
    
        
