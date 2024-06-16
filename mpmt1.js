// mpmt1.js: A Node.js version of mpmt1.py
//
//License:
//  Apache License, Version 2.0
//History:
//  * 2021/12/30 v0.1 Initial version
//Author:
//  Masanori Itoh <masanori.itoh@gmail.com>
//TODO:
//  * Implement async mode(?)

function busy_worker(timeout) {

    // ts and ts_save are in mili-seconds
    var ts_save = Date.now()
    // duration is in seconds.
    var max = timeout * 1000

    while (true) {
        ts = Date.now()
        if ((ts - ts_save) >= max) {
            console.log('Expired! ' + (ts - ts_save))
            break
        }
    }
    if (mode == 'p') {
        process.exit(0)
    }
}


var posix_getopt = require('posix-getopt')
var parser, option

const cluster = require('cluster')

var num_context = 4
var duration = 10
var mode = 'p'

parser = new posix_getopt.BasicParser('n:d:m:', process.argv)
while ((option = parser.getopt()) !== undefined) {
    switch (option.option) {
    case 'n':
        num_context = option.optarg
        break;
    case 'd':
        duration = option.optarg
        break;
    case 'm':
        mode = option.optarg
        break;
    }
}


if (mode == 'p') {
    //Process mode (cluster)
    if (cluster.isMaster) {
        console.log('num_context: ' + num_context + ' duration: ' + duration + ' mode: ' + mode)
        console.log('parent: pid= ' + process.pid)

        for (var i = 0; i < num_context; i++) {
            cluster.fork()
        }

        cluster.on('exit', (code, signal) => {
            // signal is the value for process.exit()? node code?
            console.log('on exit called. code= ' + code + ' signal= ' + signal)
        });

    } else {
        console.log('child: pid=' + process.pid)
        busy_worker(duration)
    }
} else {
    //Thread mode (worker_thread)
    const {
        Worker,
        isMainThread,
        //parentPort,
    } = require("worker_threads");
    
    if (isMainThread) {
        console.log('num_context: ' + num_context + ' duration: ' + duration + ' mode: ' + mode)
        const threads = [];
        for (var i = 0; i < num_context; i++) {
            const worker = new Worker(__filename, {argv: process.argv.slice(2)});
            threads.push(worker)
        }
        //TODO(thatsdone): Implement join() equivalent for synchronization
    } else {
        busy_worker(duration)
    } 
}    
