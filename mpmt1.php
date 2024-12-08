<?php
# mpmt1.tcl: A simple multi-threading/processing example in PHP
#
# License:
#   Apache License, Version 2.0
# History:
#   * 2024/12/07 v0.1 Initial version
# Author:
#   Masanori Itoh <masanori.itoh@gmail.com>
# Dependency:
#   * pcntl for multi-process
#   * pthreads for multi-thread
#   * parallel (maybe, not for now)
# Note:
#   * Explore 'parallel' extension usage. Note that 'parallel' extension
#     requires PHP 8.x. Also, I'm seeing an issue that parallel implemetation
#     is executed only sequentially, meaning does not use multiple cores.
#     It would be my misunderstanding, but I'm not sure why at the moment.
#     The below (outedated) pthreads implementation works as expected.
# Usage:
#   * `# php mpmt1.php [NUM_CONTEXT [DURATION [MODE]]]`
#
# References
# * https://www.php.net/manual/en/book.pcntl.php
# * https://www.php.net/manual/en/intro.parallel.php
# * https://www.php.net/manual/en/book.pthreads.php
# * https://www.php.net/manual/en/language.oop5.properties.php
#
    $num_context = 3;
    $duration = 5;
    $mode = "p";

    if (count($argv) > 1) {
        $num_context = $argv[1];
    }
    if (count($argv) > 2) {
        $duration = $argv[2];
    }
    if (count($argv) > 3) {
        $mode = $argv[3];
    }

    printf("num_context: %d duration: %d mode: %s\n", $num_context, $duration, $mode);

    $clist = array();

    if ($mode == "p") {
        for ($idx = 0; $idx < $num_context; $idx++) {
            #printf("creating...: %d\n", $idx);
            $pid = pcntl_fork();
            if ($pid === 0) {
                //child
                $ts_start = microtime(true);
                while (true) {
                    $ts_now = microtime(true);
                    if (($ts_now - $ts_start) > $duration) {
                        printf("Exiting...\n");
                        exit;
                    }
                }
            } else if ($pid === -1) {
                //error
                throw new RuntimeException('Failed to create a process');

            } else {
                //parent
                array_push($clist, $pid);
            }
        }
        foreach($clist as &$pid) {
            $res = pcntl_waitpid($pid, $status);
            #printf("%s %s %s\n", $res, $pid, $status);
        }

    } else if ($mode == "t") {
        # The below uses outdated 'pthreads', not 'parallel'.

        class Mpmt1 extends Thread
        {
            public $duration;

            public function __construct(int $duration)
            {
                $this->duration = $duration;
            }

            public function run() {
                #printf("in function. duration = %d\n", $this->duration);
                $ts_start = microtime(true);
                while (true) {
                    $ts_now = microtime(true);
                    if (($ts_now - $ts_start) > $this->duration) {
                        printf("Expired...\n");
                        break;
                    }
                }
            }
        }
        for ($idx = 0; $idx < $num_context; $idx++) {
            #printf("creating...: %d\n", $idx);
            $task = new Mpmt1($duration);
            array_push($clist, $task);
        }
        foreach($clist as &$t) {
            #printf("starting...: %s\n", $t->getThreadId());
            $t->start();
        }
        foreach($clist as &$t) {
            #printf("joining...: %s\n", $t->getThreadId());
            $t->join();
        }

    } else {
        printf("Unkown mode: %s\n", $mode);
    }
?>
