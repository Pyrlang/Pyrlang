%% Erlang -> link/monitor -> Python
%%
%% This example shows:
%% 1. Remotely killing a Pyrlang process, and a Erlang process
%% 2. Linking from Erlang to a Pyrlang process (and killing via the link)
%% 3. Monitoring from Erlang a Pyrlang process and observing it exit
%%
%% Run: `make example5a` in one terminal window, then `make example5b` in another
%%
-module(e05_links_monitors).

%% API
-export([start/0]).

start() ->
    erlang:register(example5, self()),
    erlang:process_flag(trap_exit, true),

    io:format("EXAMPLE5: Registering self as example5. Please run <make example5b> now.~n"),

    test_loop(),

    io:format("EXAMPLE5: Stopping...~n"),
    init:stop().


test_loop() ->
    receive
        {example5, test_exit, Pid1} ->
            %% Step 1: Now process P1 was created remotely and we know its Pid
            io:format("EXAMPLE5: sending exit to remote ~p...~n", [Pid1]),
            erlang:exit(Pid1, example5_exit_reason),
            test_loop();

        {example5, test_link, Pid2} ->
            io:format("EXAMPLE5: linking AND sending exit to remote ~p...~n", [Pid2]),

            erlang:link(Pid2),
            erlang:exit(Pid2, example5_link_reason),
            receive {'EXIT', Pid2, Reason2} ->
                io:format("EXAMPLE5: Remote exited with ~p~n", [Reason2])
            end,
            test_loop();

        {example5, test_monitor, Pid3} ->
            io:format("EXAMPLE5: monitoring Pyrlang ~p...~n", [Pid3]),

            Ref3Orig = erlang:monitor(process, Pid3),
            erlang:exit(Pid3, example5_monitor_reason),
            receive {'DOWN', Ref3, process, Pid3, Reason3} ->
                io:format("EXAMPLE5: Remote exited with ref=~p reason=~p; local ref=~p~n",
                          [Ref3, Reason3, Ref3Orig])
            end,
            test_loop();

        {example5, stop} ->
            ok
    end.
