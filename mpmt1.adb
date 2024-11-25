--
-- mpmt1.adb : A simpl Ada implementation of mpmt1 series.
--
--  License:
--    Apache License, Version 2.0
--  History:
--    * 2024/11/25 v0.1 Initial version
--  Author:
--    Masanori Itoh <masanori.itoh@gmail.com>
--
--  gnatmake mpmt1.adb -o mpmt1adb
--
-- * https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html
-- * https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_dates_times.html#date-and-time-handling
-- * https://stackoverflow.com/questions/34239915/how-to-get-seconds-since-unix-epoch-in-ada
-- * https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/a-calcon.ads
--
-- * variable number of tasks
--   * https://stackoverflow.com/questions/16999698/ada-multiple-tasks
-- * argc/argv
--   * https://stackoverflow.com/questions/14491899/command-line-arguments-for-ada
-- * https://learn.adacore.com/courses/intro-to-ada/chapters/tasking.html#simple-task
-- * multi-task
--   https://stackoverflow.com/questions/16999698/ada-multiple-tasks
--
with Ada.Text_IO; use ADA.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Strings;
with Ada.Real_Time; use Ada.Real_Time;

procedure mpmt1 is
    --
    -- body of workload (busy_loop)
    --
    procedure busy_loop (dur : Integer) is
        start : Time := Clock;
        D : Time_Span := Seconds(dur);
        now : Time;
    begin
        loop
            now := Clock;
            if To_Duration(now - start) > To_Duration(D) then
                Put_line("Expired. " & Integer'Image(dur));
                return;
            end if;
         end loop;
    end busy_loop;
    --
    -- argument parser
    --
    function ParseArgIdx (idx : Integer) return Integer is
    begin
      if Ada.Command_Line.Argument_count >= idx then
          return Integer'Value (Ada.Command_line.Argument(idx));
      else
          if idx = 1 then
              return 4;
          elsif idx = 2 then
              return 5;
          else
              return 0;
          end if;
      end if;
    end ParseArgIdx;
    --
    -- variables
    --
    num_context : Integer := ParseArgIdx(1);
    duration : Integer := ParseArgIdx(2);
    --
    -- task definition
    --
    task type worker_tasks;
    task body worker_tasks is
    begin
        busy_loop(duration);
    end worker_tasks;
    type task_array is array (Integer range <>) of worker_tasks;
    worker_task_array : task_array(1 .. num_context);

begin
    Put_Line("num_context: " & Integer'Image(num_context) & " duration: " & Integer'Image(duration));
    --null;
end mpmt1;
