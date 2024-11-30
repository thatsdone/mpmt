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
-- TODO
-- * Look for dynamic thread(task) creation syntax like pthread_create().
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
    -- Here we get default or given values as command line arguments
    -- via a function, ParseArgIdx(), because here cannot be statements
    --  such as 'if' clause as this is declare block.
    num_context : Integer := ParseArgIdx(1);
    duration : Integer := ParseArgIdx(2);
    --
    -- task declaration
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
    --
    -- If the above message does not exist, the below 'null;' is necessary.
    --
    --null;
end mpmt1;
