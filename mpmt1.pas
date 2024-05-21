//
//  mpmt1.pas: A stupid simple example of Pascal
//
// License:
//   Apache License, Version 2.0
// History:
//   * 2024/05/20 v0.1 Initial version
// Author:
//   Masanori Itoh <masanori.itoh@gmail.com>
// Usage:
//-  Install FreePascal
//-  $ fpc -ompmt1pas mpmt1.pas
//-  $ ./mpmt1pas  NUM_CONTEXT (number of threads) DURATION (in sec.)
// TODO:
//   * Use argparse
//   * Use (at least) mili-second precision expiration check
//
program mpmt1;

uses cthreads, sysutils, dateutils;

function busy_worker(duration : pointer) : IntPtr;
var
   start_ts : longint;
   current_ts: longint;
begin
   // writeLn('thread ', longint(p), ' started.');
   //DateTimeToUnix(Now) returns timestamp in second
   start_ts := DateTimeToUnix(Now);
   while true do
      begin
         current_ts := DateTimeToUnix(Now);
         if (current_ts - start_ts) > ptruint(duration) then
            begin
               writeln('Expired.');
               exit(0);
            end;
      end;
   //never reach here
   exit(0);
end;

var
   i           : longint;
   duration    : ptruint;
   num_context : longint;
   pidarray    : array of TThreadID;
begin
   duration := 10;
   num_context := 3;
   if paramcount() >= 2 then
      duration := StrToInt(paramStr(2));
   if paramcount() >= 1 then
      num_context := StrToInt(paramStr(1));

   writeln('NUM_CONTEXT: ', num_context, ' DURATION: ', duration);

   //for i:=1 to paramCount() do
   //   Writeln('Opt: ', i, ' = ', StrToInt(paramStr(i)));

   setLength(pidarray, num_context);
   for i:=1 to num_context do
   begin;
      writeln('Creating Thread: ', i);
      pidarray[i] := BeginThread(@busy_worker, pointer(duration));
   end;
   for i:=1 to num_context do
   begin;
      WaitForThreadTerminate(pidarray[i], (duration + 1) * 1000);
      writeln('Thread: ', i, ' returned.');
   end;
end.

