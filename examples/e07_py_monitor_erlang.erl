%% Python -> monitor -> Erlang
%%
%% This example shows:
%% 1. Monitoring an Erlang process from Python and killing it remotely
%% 2. A ``{'DOWN', Ref, 'process', Pid, Reason}`` message will be delivered to
%%      the Pyrlang process which was monitoring the Erlang process.
%%
%% Run: `make example7a` in one terminal window, then `make example7b` in another
%%
-module(e07_py_monitor_erlang).

%% API
-export([start/0]).

start() ->
    erlang:register(example7, self()),
    erlang:process_flag(trap_exit, true),

    io:format("EXAMPLE7: Registering self as example7. Please run <make example7b> now.~n"),

    test_loop(),

    io:format("EXAMPLE7: Stopping...~n"),
    init:stop().


helper_process_loop(0) ->
    ok;
helper_process_loop(Count) ->
    receive stop ->
        io:format("Dying on request~n"),
        ok
    after 400 ->
        io:format("Helper process: ~p; ~p~n",
                  [erlang:process_info(self(), links),
                   erlang:process_info(self(), monitored_by)])
    end,
    helper_process_loop(Count - 1).


test_loop() ->
    receive
        {example7, test_monitor, PyPid2} ->
            ErlPid2 = erlang:spawn_link(fun() -> helper_process_loop(15) end),
            io:format("EXAMPLE7: Spawned a process ~p for Pyrlang pid ~p...~n",
                      [ErlPid2, PyPid2]),

            PyPid2 ! {test_monitor, ErlPid2},
            test_loop();

        {'EXIT', Pid3, Reason3} ->
            io:format("EXAMPLE7: Pid ~p exited with ~p~n", [Pid3, Reason3]),
            test_loop();

        {example7, stop} ->
            ok
    end.
