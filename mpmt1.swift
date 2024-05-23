// mpmt1.swift: A stupid simple example of Swift threading (async function)
//
// License:
//   Apache License, Version 2.0
// History:
//   * 2024/05/22 v0.1 Initial version
// Author:
//   Masanori Itoh <masanori.itoh@gmail.com>
// TODO:
//   * Add HELP messages.
// References:
//   * https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency
//   * https://stackoverflow.com/questions/27521832/create-dynamic-size-array-swift
//
import Foundation


func busyWorker(_ duration: Int!) async {
    let start_ts = Date().timeIntervalSince1970
    while true {
        let now_ts = Date().timeIntervalSince1970
        if (now_ts - start_ts) > CGFloat(duration) {
            print("Expired.")
            return
        }
    }
}

// default values
var num_context:Int? = 3
var duration:Int? = 5

// parse arguments and update parameters
for idx in 0..<CommandLine.arguments.count {
    switch idx {
    case 1:
        num_context = Int(CommandLine.arguments[idx])
        break
    case 2:
        duration = Int(CommandLine.arguments[idx])
        break
    default:
        break
    }
}

print("NUM_CONTEXT: \(num_context!) DURATION: \(duration!)")

let task = Task {
    try await withThrowingTaskGroup(of: Int.self) { group in
        for idx in 1...num_context! {
            group.addTask {
                print("Creating \(idx) th worker.")
                //print(idx)
                await busyWorker(duration)
                return 0
            }
        }
        try await group.waitForAll()
    }
}

_ = await task.result

