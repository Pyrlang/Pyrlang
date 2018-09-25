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
            receive {'EXIT', Pid2, Reason} ->
                io:format("EXAMPLE5: Remote exited with ~p~n", [Reason])
            end,
            test_loop();

        {example5, stop} ->
            ok
    end.