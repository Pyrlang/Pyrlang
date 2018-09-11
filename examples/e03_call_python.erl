%% For this demo please make sure that `make pynode` is running in a separate
%% console tab.

-module(e03_call_python).

%% API
-export([start/0]).

start() ->
    %% Create a remote notebook object (context) on Python side
    io:format("EXAMPLE3: Connecting to py@127.0.0.1~n"),
    Ctx = py:new_context('py@127.0.0.1'),

    %% Import datetime and call datetime.now(), kwargs are empty by default.
    %% Note that binaries, ASCII strings and atoms all work.
    %% First element in the list is module name to be imported
    DT1 = py:call(Ctx, [<<"datetime">>, "datetime", now], []),
    timer:sleep(2000),

    %% Import datetime and call datetime.now() again but now with kwargs #{}
    DT2  = py:call(Ctx, [datetime, datetime, <<"now">>], [], #{}),

    %% Subtract two datetimes
    Diff = py:call(Ctx, [DT2, '__sub__'], [DT1], #{}),

    %% Call Diff.total_seconds() and retrieve the value without storing it
    %% in remote history.
    Result1 = py:call(Ctx, [Diff, <<"total_seconds">>], [], #{}, #{immediate => true}),
    io:format("EXAMPLE3: Result1 (immediate) = ~p~n", [Result1]),

    %% Or retrieve the diff and store it remotely then retrieve
    Result2Ref = py:call(Ctx, [Diff, <<"total_seconds">>], []),
    Result2 = py:retrieve(Ctx, Result2Ref),
    io:format("EXAMPLE3: Result2 (retrieved) = ~p~n", [Result2]),

    %% Done with the remote context. Remote notebook object will be dropped.
    py:destroy(Ctx),
    io:format("Finished~n"),
    init:stop().
