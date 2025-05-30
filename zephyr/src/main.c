/*
 * zephyr/src/main.c: Zephyr version of mpmt1
 *
 * STATUS:
 *  Under development
 * License:
 *   Apache License, Version 2.0
 * History:
 *   2025/05/27 v0.1 Initial version
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 * BUILD & RUN:
 *   * Initialize your zephyr project
 *     * Tested west 1.4.0 and zephyr-sdk-0.17.0 on Ubuntu 24.04(arm64)
 *   * copy mpmt/zephyr into your zephyr project as mpmt1
 *   * `$ west build -p always -b qemu_qemu_x86_64 mpmt1`
 *     or
 *   * `$ west build -p always -b qemu_kvm_arm64 mpmt1`
       then
 *   * `$ west build -t run`
 *     Otherwise, you can run build/zephyr/zephyr.elf on qemu with
 * REFERENCES:
 *   * https://docs.zephyrproject.org/latest/develop/getting_started/index.html
 *   * https://docs.zephyrproject.org/latest/boards/qemu/x86/doc/index.html
 *   * https://docs.zephyrproject.org/latest/boards/qemu/kvm_arm64/doc/index.html
 * TODO:
 *   * Consinder thread priority (cooperative or preempt?)
 *   * Look into kernel thread joining method
 *   * Consider using device tree for externally given variables
 *   * ...
 */
#include <zephyr/kernel.h>
#include <stdio.h>
#include <stdlib.h>
#include <zephyr/sys/printk.h>
#include <zephyr/console/console.h>

#define T_STACK_SIZE (1280 + 1024)
#define NUM_THREADS 16
static K_THREAD_STACK_ARRAY_DEFINE(thread_stack, NUM_THREADS, T_STACK_SIZE);
static struct k_thread threads[NUM_THREADS];


void busy_worker(void *arg1, void *arg2, void *arg3)
{
  int duration = (int)arg1;
  int tid = (int)arg2;
  uint32_t now1;
  uint32_t now2;
  uint32_t dur;
  //
  uint32_t vfactor = 1;
  
  printk("busy_worker called. tid: %d duration: %d\n", tid, duration);

  now1 = k_cycle_get_32();
  dur = sys_clock_hw_cycles_per_sec() * duration / vfactor; 
  printk("duration: %d dur: %d\n", duration, dur);
  while (true) {
    now2 = k_cycle_get_32();
    if ((now2 - now1) >=  dur) {
      printk("Expired \n");
      break;
    }
  }
  
  return;
}

int main(void)
{
  int duration = 0;
  int num_context = 0;
  int i;
  uint32_t start;
  uint32_t end;
  char *s = NULL;

  console_getline_init();
  
  printk("mpmt1 for Zephyr: target: %s num_cpus: %d\n", CONFIG_BOARD_TARGET, arch_num_cpus());

  printk("input num_context: ");
  s = console_getline();
  num_context = atoi(s);
  if (num_context < 1) {
    num_context = CONFIG_MPMT1_NUM_CONTEXT;
  }
  printk("input duration: ");
  s = console_getline();
  duration = atoi(s);
  if (duration < 1) {
    duration = CONFIG_MPMT1_DURATION;
  }

  printk("num_context: %d duration: %d\n", num_context, duration);

  start = k_cycle_get_32();
  for (i = 0; i < num_context; i++) {
    k_thread_create(&threads[i],
		    thread_stack[i],
		    T_STACK_SIZE,
		    busy_worker,
		    (void *)duration, (void *)i, NULL,
		    K_PRIO_PREEMPT(10), 0, K_NO_WAIT);
  }

  for (i = 0; i < num_context; i++) {
    k_thread_join(&threads[i], K_FOREVER);
  }

  end = k_cycle_get_32();
  printk("total duration(ms): %lld\n", k_cyc_to_ms_floor64(end - start));
  printk("mpmt1 finished.\n");
  return 0;
}
