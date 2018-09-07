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
    destroy/1,
    get_type/1,
    new_context/1,
    new_context/2,
    retrieve/2
]).

%% Creates a remote notebook object which will handle the calls on this context
-record(pyrlang_ctx, {
    node :: node(),
    remote_pid :: pid(),
    ref = erlang:make_ref() :: reference() % to identify context in value refs
}).

%% Refers to a remote value saved as integer index automatically or a named one
-record(pyrlang_value_ref, {
    id :: binary() | integer(),
    %% Ref is used to match valueref against context which created it
    ref :: reference(),
    %% Used to easier detect remote errors when a different type was expected
    remote_type :: string() | binary()
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


destroy(#pyrlang_ctx{remote_pid = Pid}) ->
    undefined = gen_server:call(Pid, {exit, normal}),
    ok.


%% @doc Accessor to the type field
get_type(#pyrlang_value_ref{remote_type = T}) -> T.


%% @doc Perform a remote call with default timeout of 5s and no keyword args
call(Ctx, Path, Args) -> call(Ctx, Path, Args, #{}).

%% @doc Perform a remote call with default timeout of 5s
call(Ctx, Path, Args, KeywordArgs) ->
    call(Ctx, Path, Args, KeywordArgs, #{timeout => 5000}).

%% @doc Perform a remote call. A value reference id is returned, which is a
%% numeric index in remote notebook history. Value reference can be reused as
%% an argument in upcoming calls.
%% Options can have keys:
%%  *   timeout: gen_server:call timeout, ms
%%  *   immediate (bool): if true, the {value, Result} will be returned,
%%      otherwise it will be stored remotely and you get a #pyrlang_value_ref{}
call(#pyrlang_ctx{remote_pid = Pid,
                  ref        = CtxRef},
     Path, Args, KeywordArgs, Options
) ->
    Timeout = maps:get(timeout, Options, 5000),
    Immediate = maps:get(immediate, Options, false),

    %% For path first element - it might be a value reference
    Path1 = [convert_valueref(erlang:hd(Path)) | erlang:tl(Path)],

    %% Any arg can be a value reference
    Args1 = lists:map(fun convert_valueref/1, Args),

    %% Any value of kwargs can be a value reference: unzip, convert, zip back
    {KwargsKeys, KwargsValues} = lists:unzip(maps:to_list(KeywordArgs)),
    KwargsValues1 = lists:map(fun convert_valueref/1, KwargsValues),
    KeywordArgs1 = maps:from_list(lists:zip(KwargsKeys, KwargsValues1)),

    case gen_server:call(Pid,
                         {nb_call, #{path => Path1,
                                     args => Args1,
                                     kwargs => KeywordArgs1,
                                     immediate => Immediate}},
                         Timeout)
    of
        {ok, Type, VRef} -> % value stored remotely
            #pyrlang_value_ref{id          = VRef,
                               remote_type = Type,
                               ref         = CtxRef};
        {value, V} ->
            V % immediate return, remote history was not updated
    end.


%% @doc Given a value, replaces #pyrlang_value_ref{} records with simpler tuples
convert_valueref(#pyrlang_value_ref{id = Id}) -> {'$pyrlangval', Id};
convert_valueref(X) -> X.


%% @doc Retrieve a remote value with id stored in second argument.
%% Error {error, not_found} means the value does not exist/was cleared when
%% history limit was reached.
-spec retrieve(#pyrlang_ctx{}, #pyrlang_value_ref{}) ->
    {ok, any()} | {error, not_found}.
retrieve(#pyrlang_ctx{remote_pid = Pid,
                      ref        = CtxRef},
         #pyrlang_value_ref{id  = Id,
                            ref = CtxRef}) ->
    gen_server:call(Pid, {nb_retrieve, Id});

retrieve(#pyrlang_ctx{}, #pyrlang_value_ref{}) ->
    erlang:error({error, ref_does_not_match}).
