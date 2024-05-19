%
%  mpmt1.erl: A stupid simple example of Erlang threading
%
% License:
%   Apache License, Version 2.0
% History:
%   * 2024/05/18 v0.1 Initial version
% Author:
%   Masanori Itoh <masanori.itoh@gmail.com>
% Usage:
%  $ erlc mpmt1.erl
%  $ erl -noshell -pa mpmt1.beam  -s mpmt1 start NUM_CTX DURATION -s init stop
%  NUM_CTX : number of threads
%  DURATION : duration in seconds
% TODO:
%   * Enable to flush remaining messages in busy_worker()
%   * Use argparse
%   * Use message to synchronize waiting for thread termination
%
-module(mpmt1).
-export([start/1, busy_worker/3, create_busy_worker/2]).

busy_worker(Idx, Current_ns, Timeleft_ns) ->
    if
        Timeleft_ns =< 0 -> 
            io:format("Expired: ~p ~p ~n", [Idx, Timeleft_ns]),
            %exit(normal);
            ok;
        true -> 
            Now = erlang:system_time(),
            busy_worker(Idx, Now, Now - Current_ns)
    end.

create_busy_worker(0, _) ->
    ok;
create_busy_worker(Num_context, Duration) ->
    io:format("create_busy_worker(): Idx: ~p~n", [Num_context]),
    % system_time() returns the timestamp in nano-seconds
    Now = erlang:system_time(),
    spawn(mpmt1, busy_worker, [Num_context, Now, Duration * 1000 * 1000 * 1000]),
    create_busy_worker(Num_context - 1, Duration).

start(Args) ->
    Duration =
        if
            length(Args) >=2  ->
                erlang:list_to_integer(atom_to_list(lists:nth(2, Args)));
            true ->
                5
        end,

    Num_context =
        if
            length(Args) >=1  ->
                erlang:list_to_integer(atom_to_list(lists:nth(1, Args)));
            true ->
                4
        end,
    
    io:format("NUM_CONTEXT: ~p DURATION: ~p~n", [Num_context, Duration]),

    create_busy_worker(Num_context, Duration),

    % TODO(thatsdone): Use message to synchronize with threads.
    io:format("start(): sleeping ~p sec.~n", [Duration + 1]),
    timer:sleep((Duration + 1) * 1000).

