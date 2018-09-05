%%% @copyright 2018, S2HC and Erlang Solutions Ltd.
%%% @doc Helper module to support advanced RPC call styles with Pyrlang library
%%%     for Python. This works notebook-style (think Jupyter) where you do
%%%     multiple calls, their results are saved as #1, #2, #3 etc and then also
%%%     you can refer to them without having to transfer back to Erlang node.
%%% @end

-module(py).

%% API
-export([
    call/3,
    call/4,
    call/5,
    new_context/1,
    new_context/2,
    retrieve/2
]).

%% Creates a remote notebook object which will handle the calls on this context
-record(pyrlang_ctx, {
    node :: node(),
    remote_pid :: pid()
}).

%% Refers to a remote value saved as integer index automatically or a named one
-record(pyrlang_value_ref, {
    id :: binary() | integer()
}).


new_context(Node) -> new_context(Node, #{}).

%% @doc Creates a new notebook context for performing remote calculations on
%% Python node. Intermediate results are stored remotely until they are
%% requested.
%% For options documentation refer to Pyrlang docs.
%% Option:  history :: int() - sets history limit size, older values are deleted
%%          Default history size: 50
-spec new_context(node(), map()) -> #pyrlang_ctx{}.
new_context(Node, Options) ->
    RemotePid = rpc:call(Node, 'Pyrlang.Notebook.notebook', new_context, [Options]),
    #pyrlang_ctx{
        node = Node,
        remote_pid = RemotePid
    }.


%% @doc Perform a remote call with default timeout of 5s and no keyword args
call(Ctx, Path, Args) -> call(Ctx, Path, Args, #{}, 5000).

%% @doc Perform a remote call with default timeout of 5s
call(Ctx, Path, Args, KeywordArgs) -> call(Ctx, Path, Args, KeywordArgs, 5000).

%% @doc Perform a remote call. A value reference id is returned, which is a
%% numeric index in remote notebook history. Value reference can be reused as
%% an argument in upcoming calls.
call(#pyrlang_ctx{remote_pid = Pid}, Path, Args, KeywordArgs, Timeout) ->
    case gen_server:call(Pid, {call, #{path => Path,
                                       args => Args,
                                       kwargs => KeywordArgs}}, Timeout)
    of
        {ok, VRef} -> #pyrlang_value_ref{id = VRef}
    end.


%% @doc Retrieve a remote value with id stored in second argument.
%% Error {error, not_found} means the value does not exist/was cleared when
%% history limit was reached.
-spec retrieve(#pyrlang_ctx{}, #pyrlang_value_ref{}) ->
    {ok, any()} | {error, not_found}.
retrieve(#pyrlang_ctx{remote_pid = Pid}, #pyrlang_value_ref{id = Id}) ->
    gen_server:call(Pid, {retrieve, Id}).
