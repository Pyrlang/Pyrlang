%% For this demo please make sure that `make pynode` is running in a separate
%% console tab.

-module(e04_batch_call_python).

%% API
-export([start/0]).

start() ->
    %% Create an empty batch and begin adding calls to it
    S0 = py:batch_new(),
    {S1, R1} = py:batch_call(S0, [<<"datetime">>, "datetime", now], []),
    {S2, R2} = py:batch_call(S1, [datetime, datetime, <<"now">>], [], #{}),
    %% Subtract two datetimes
    {S3, Diff} = py:batch_call(S2, [R2, '__sub__'], [R1], #{}),

    %% Call Diff.total_seconds() and retrieve the value without storing it
    %% in remote history.
    {S4, _R4} = py:batch_call(S3, [Diff, <<"total_seconds">>], []),
    io:format("EXAMPLE4: Batch created:~n~120p~n", [S4]),

    %% Create a remote notebook object (context) on Python side
    io:format("EXAMPLE4: Connecting to py@127.0.0.1~n"),
    Ctx  = py:new_context('py@127.0.0.1'),

    %% will retrieve because immediate=true by default
    Result = py:batch_run(Ctx, S4),
    io:format("EXAMPLE4: Result1 (immediate) = ~p~n", [Result]),

    Result2Ref = py:batch_run(Ctx, S4, #{immediate => false}),
    Result2 = py:retrieve(Ctx, Result2Ref),
    io:format("EXAMPLE4: Result2 (retrieved) = ~p~n", [Result2]),
    io:format("NOTE: Result2 will be different from Result1 because the batch "
                ++ "was executed once again.~n"),

    %% Done with the remote context. Remote notebook object will be dropped.
    py:destroy(Ctx),
    io:format("Finished~n"),
    init:stop().
