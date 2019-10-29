%% Python -> monitor -> Erlang
%%
%% This example shows:
%% 1. Monitoring an Erlang process from Python and killing it remotely
%% 2. A ``{'DOWN', Ref, 'process', Pid, Reason}`` message will be delivered to
%%      the Pyrlang process which was monitoring the Erlang process.
%%
%% Run: `make example7a` in one terminal window, then `make example7b` in another
%%
-module(e08_multiple_py_nodes).

%% API
-export([start/0]).

start() ->
    erlang:register(example8, self()),
    erlang:process_flag(trap_exit, true),

    io:format("EXAMPLE8: Registering self as example8. Please run <make example8b> and <make example8c> now.~n"),

    test_loop(undefined, undefined),

    io:format("EXAMPLE8: Stopping...~n"),
    init:stop().

test_loop(Py1, Py2) when is_pid(Py1), is_pid(Py2) ->
    io:format("both py nodes registered, time for next step~n"),
    io:format("casting py server pids to one another~n"),
    gen_server:cast(Py1, {other_py_node, Py2}),
    % it doesn't work if we try to connect both at the same time
    % probably need to implement the backof of node 2 as the handshake
    % is being done
    % gen_server:cast(Py2, {other_py_node, Py1}),
    io:format("calling the 2 pynodes to get response"),
    R1 = gen_server:call(Py1, who_are_you),
    io:format("response from py1 ~p~n", [R1]),
    R2 = gen_server:call(Py2, who_are_you),
    io:format("response from py2 ~p~n", [R2]),
    % This one works though
    gen_server:cast(Py2, {other_py_node, Py1}),
    ok;
test_loop(Py1, Py2) ->
    receive
        {register, Pid} when is_pid(Pid) ->
            case node(Pid) of
                'py1@127.0.0.1' ->
                    io:format("register request form py1~n"),
                    test_loop(Pid, Py2);
                'py2@127.0.0.1' ->
                    io:format("register request form py2~n"),
                    test_loop(Py1, Pid);
                Node ->
                    io:format("register request form ~p~n", [Node]),
                    test_loop(Py1, Py2)
            end;
        Other ->
            io:format("got ~p~n", [Other]),
            test_loop(Py1, Py2)
    end.
