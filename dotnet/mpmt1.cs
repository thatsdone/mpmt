/*
 * mpmt1.cs: A stupid simple example of C# threading
 *
 * License:
 *   Apache License, Version 2.0
 * History:
 *   2024/05/26 v0.1 Initial version based on:
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 * Usage:
 *   * prepare dotnet sdk/runtime
 *      * e.g., mcr.microsoft.com/dotnet/sdk:8.0
 *   * `$ cd dotnet`
 *   * `$ dotnet restore`
 *   * `$ dotnet build`
 *   * `$ bin/Debug/net8.0/mpmt1 NUM_CONTEXT DURATION`
 * TODO:
 *   * Use getopt
 * References:
 *   * https://www.geeksforgeeks.org/c-sharp-multithreading/
 */
using System;
using System.Threading;
using System.Collections.Generic;

class Program
{
  static void busy_worker(object duration) {
    Console.WriteLine("busy_worker() called."); 
    long start_ts = DateTimeOffset.Now.ToUnixTimeMilliseconds();
    while (true) {
      long current_ts = DateTimeOffset.Now.ToUnixTimeMilliseconds();
      if (current_ts - start_ts > (long)duration) {
        Console.WriteLine("Expired.");
        break; 
      }
    }
    return;
  }

  static void Main(string[] args)
  {
    List<Thread>tlist = new List<Thread>();
    long duration = 5;
    long num_context = 3;

    if (args.Length >= 1) {
      num_context = Int32.Parse(args[0]);
    }
    if (args.Length >= 2) {
      duration = Int32.Parse(args[1]);
    }
    Console.WriteLine("NUM_CONTEXT: {0} DURATION: {1}", num_context, duration);

    for (int i = 0; i < num_context; i++) {
      tlist.Add(new Thread(busy_worker)); 
    }

    foreach (Thread t in tlist) {
      t.Start(duration * 1000);
    }

    foreach (Thread t in tlist) {
      t.Join();
    }   
  }
}
