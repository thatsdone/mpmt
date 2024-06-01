//
//  mpmt1.dart: A stupid simple example of Dart
//
// License:
//   Apache License, Version 2.0
// History:
//   * 2024/05/24 v0.1 Initial version
// Author:
//   Masanori Itoh <masanori.itoh@gmail.com>
// Usage:
//-  Prepare Dart sdk environemt such as docker Dart image
//-  $ dart pub get
//-  $ dart mpmt1.dart NUM_CONTEXT (number of threads) DURATION (in sec.)
// TODO:
//   * Use args
//   * Implement worker and main threads synchroniation
//
import 'package:sprintf/sprintf.dart';
import 'dart:async';
import 'dart:isolate';

void busy_worker(int duration_ms) async {    
    print("busy_worker() called.");
    int start_ts = DateTime.now().millisecondsSinceEpoch;
    while (true) {
        int current_ts = DateTime.now().millisecondsSinceEpoch;
        if ((current_ts - start_ts) >= duration_ms) {
            print("Expired.");
            break;
        }
    }
    return;
}

void main(List<String> args) async {
    int num_context = 3;
    int duration = 5;

    if (args.length >= 1) {
        num_context = int.parse(args[0]);
    }
    if (args.length >= 2) {
        duration = int.parse(args[1]);
    }
    
    print(sprintf("NUM_CONTEXT: %d DURATION: %d", [num_context, duration]));

    List<Future<int>> futures = [];
    for (int i = 0; i < num_context; i++) {
        Isolate.spawn(busy_worker, duration * 1000);
    }
    new Future.delayed(new Duration(seconds: duration + 1 )).then((_) => print("completed."));
}
    