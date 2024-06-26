package com.github.thatsdone.mpmt;
/**
 * Mpmt1 : mpmt1.py Java 19 version. You can use Virtual Thread by specifying
 * '-v'
 *
 * License:
 *   Apache License, Version 2.0
 * History:
 *   * 2023/10/06 v0.1 Initial version
 * Author:
 *   Masanori Itoh <masanori.itoh@gmail.com>
 */
import java.util.*;
import java.io.*;
import java.lang.Runtime;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.HelpFormatter;

public class Mpmt1
{

    private static class worker implements Runnable {
	int index = -1;
	long duration = 5 * 1000L;

	worker(int i, long duration) {
	    this.index = i;
	    this.duration = duration;
	}

        @Override
        public void run() {
	    long ts_orig = System.currentTimeMillis();
	    System.out.println("worker::run: ts: " + ts_orig
			       + " index = " + index
			       + " isVirtual: "
			       + Thread.currentThread().isVirtual());
            long ts;
	    int count = 0;
            while (true) {
                ts = System.currentTimeMillis();
                if ((ts - ts_orig) > this.duration) {
                    System.out.println("worker::run: " + this.index + " expired. " + count);
                    return;
                }
                count++;
		//This does not have any effect for Virtual Thread?
		//Thread.currentThread().yield();
            }
        }
    }

    public static void main(String[] args) throws Exception
    {
        int i;
        int num_context = 4;
        int duration = 5;
        boolean vthread = false;
        List<Thread> workers  = new ArrayList<Thread>();
        long sleep = 0;

        Options options = new Options();
        Option help = new Option("help", "print this message");
        CommandLineParser parser = new DefaultParser();
        CommandLine cmd = null;

        options.addOption(help);
        options.addOption("n", true, "Number of threads");
        options.addOption("d", true, "Duration(sec.)");
        options.addOption("v", false, "Use Java 19 Virtual Thread");
        options.addOption("s", true, "Sleep(sec.)");

        cmd = parser.parse(options, args);
        if (cmd.hasOption("help")) {
            HelpFormatter formatter = new HelpFormatter();
            formatter.printHelp("Mpmt1", options);
            System.exit(0);
        }
        if (cmd.hasOption("n")) {
	    num_context = Integer.parseInt(cmd.getOptionValue("n"));
	}
        if (cmd.hasOption("d")) {
	    duration = Integer.parseInt(cmd.getOptionValue("d"));
	}
        if (cmd.hasOption("v")) {
	    vthread = true;
	}
        if (cmd.hasOption("s")) {
	    sleep = Integer.parseInt(cmd.getOptionValue("s"));
	}

	String vp = System.getProperty("jdk.virtualThreadScheduler.parallelism");
	String vt = System.getProperty("jdk.virtualThreadScheduler.maxPoolSize");
	String mr = System.getProperty("jdk.virtualThreadScheduler.minRunnable");
	System.out.printf("main: parallelism: %s maxPollSize: %s minRunnable: %s\n",
			  vp, vt, mr);
	System.out.printf("main: availableProcessors: %d maxMemory: %d totalMemory: %d freeMemory: %d\n",
			  Runtime.getRuntime().availableProcessors(),
			  Runtime.getRuntime().maxMemory(),
			  Runtime.getRuntime().totalMemory(),
			  Runtime.getRuntime().freeMemory()
			  );

        for (i = 0; i < num_context; i++) {
	    worker w = new worker(i, duration * 1000L);
            Thread t = new Thread(w);
            workers.add(t);
        }

        for (i = 0; i < num_context; i++) {
            System.out.println("main: starting: " + i);
	    Thread t = workers.get(i);
	    if (vthread) {
		Thread.ofVirtual().start(t);
	    } else {
		t.start();
	    }
        }

	if (vthread) {
	    System.out.println("main: Virtual Thread mode. Sleeping...");
	    Thread.sleep(sleep * 1000L);
	}

	System.out.println("main: Calling join() to each thread.");
        for (i = 0; i < num_context; i++) {
            Thread t = workers.get(i);
            t.join();
            System.out.println("main: thread join() returned. " + i + " " + t);
        }
    }
}
