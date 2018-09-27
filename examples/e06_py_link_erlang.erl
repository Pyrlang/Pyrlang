%% Python -> link -> Erlang
%%
%% This example shows:
%% 1. Linking to an Erlang process from Python and killing it remotely
%% 2. An exit message will be delivered to Pyrlang PID which was linked to it.
%%
%% Run: `make example6a` in one terminal window, then `make example6b` in another
%%
-module(e06_py_link_erlang).

%% API
-export([start/0]).

start() ->
    erlang:register(example6, self()),
    erlang:process_flag(trap_exit, true),

    io:format("EXAMPLE6: Registering self as example6. Please run <make example6b> now.~n"),

    test_loop(),

    io:format("EXAMPLE6: Stopping...~n"),
    init:stop().


helper_process_loop(0) ->
    ok;
helper_process_loop(Count) ->
    receive stop -> ok
    after 400 ->
        io:format("Helper process: ~p; ~p~n",
                  [erlang:process_info(self(), links),
                   erlang:process_info(self(), monitored_by)])
    end,
    helper_process_loop(Count - 1).


test_loop() ->
    receive
        {example6, test_link, PyPid1} ->
            %% Run loop for 15x400ms = 8sec
            ErlPid1 = erlang:spawn_link(fun() -> helper_process_loop(15) end),
            io:format("EXAMPLE6: Spawned a process ~p for Pyrlang pid ~p...~n",
                      [ErlPid1, PyPid1]),

            PyPid1 ! {test_link, ErlPid1},
            test_loop();

        {'EXIT', Pid3, Reason3} ->
            io:format("EXAMPLE6: Pid ~p exited with ~p~n", [Pid3, Reason3]),
            test_loop();

        {example6, stop} ->
            ok
    end.
