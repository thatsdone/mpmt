/*
 * mpmt1.cpp: A simple example of C++ threading and multiprocessing.
 *
 * License:
 *   Apache License, Version 2.0
 * History:
 *   2024/06/02 v0.1 Initial version
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 * BUILD:
 *   * `$ g++ -o mpmt1cpp mpmt1.cpp`
 * TODO:
 *   * Use getopt
 *   * Write multi process version.
 */
#include <iostream>
#include <thread>
#include <string>
#include <chrono>
#include <list>
#include <unistd.h>

using namespace std;
using namespace std::chrono;

void busy_worker(uint64_t duration_ms) {
  
  uint64_t start_ts = duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
  while (true) {
    uint64_t current_ts = duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
    if (current_ts - start_ts >= duration_ms) {
      cout << "Expired."  << endl;
      return;
    }
  }
}


int main(int argc, char **argv){
  int num_context = 3;
  int duration = 5;
  list<thread>tlist;

  if (argc >= 2) {
    num_context = stoi(argv[1]);
  }
  if (argc >= 3) {
    duration = stoi(argv[2]);
  }
  printf("num_context: %d duration: %d\n", num_context, duration);

  for (int i = 0; i < num_context; i++) {
    thread t = thread(busy_worker, duration * 1000);
    tlist.push_back(std::move(t));
  }
  //sleep(duration + 1);

  std::list<thread>::iterator it;
  for (it = tlist.begin(); it != tlist.end(); ++it){
    //cout << "joining..." << endl;
    (*it).join();
  }
  return 0;
}
