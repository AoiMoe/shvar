-module(shvar).

-compile([no_auto_import]).

-export([
         init/1,
         init/0,
         init_link/1,
         init_link/0,
         uninit/1,
         uninit/0,
         id/2,
         get/1,
         set/2,
         get_pool/1,
         default_namespace/0,
         to_namespace/1,
         to_key/1,
         run/2,
         foldmap/2,
         map/2
        ]).

-export_type([
              namespace/0,
              key/0,
              val/0,
              id/0,
              full_id/0,
              pool/0
             ]).

-include("include/impl.hrl").

-define(MSG(From, Msg), {?MODULE, From, Msg}).
-define(COND(Cond, Then, Else), case Cond of true -> Then; false -> Else end).

-record('$shvar_full_id',
        {
            namespace :: namespace(),
            key :: key()
        }).

-opaque full_id() :: #'$shvar_full_id'{}.

-opaque pool() :: map().

%%================================================================================
%% exported functions
%%================================================================================
%%========================================
%% main api of shvar server
%%========================================
%% @doc start shvar server.
-spec init(namespace()) -> {ok, pid()} | {error, already_started}.
init(Namespace) ->
    init_impl(Namespace, []).

%% @equiv init(default_namespace())
-spec init() -> {ok, pid()} | {error, already_started}.
init() ->
    init(?MODULE:default_namespace()).

%% @doc start shvar server with link.
-spec init_link(namespace()) -> {ok, pid()} | {error, already_started}.
init_link(Namespace) ->
    init_impl(Namespace, [link]).

%% @equiv init_link(default_namespace())
-spec init_link() -> {ok, pid()} | {error, already_started}.
init_link() ->
    init_link(?MODULE:default_namespace()).

%% @doc stop shvar server.
-spec uninit(namespace()) -> ok.
uninit(Namespace) ->
    ok = send(exit, Namespace).

%% @equiv uninit(default_namespace())
-spec uninit() -> ok.
uninit() ->
    uninit(?MODULE:default_namespace()).

%% @doc make full id.
-spec id(namespace(), key()) -> full_id().
id(Namespace, Key) ->
    #'$shvar_full_id'{namespace = Namespace, key = Key}.

%% @doc get value of the variable specified by Id.
-spec get(id()) -> val().
get(Id) ->
    foldmap(fun(Val0) -> {Val0, Val0} end, Id).

%% @doc set Val into the variable specified by Id.
-spec set(val(), id()) -> ok.
set(Val, Id) ->
    ok = foldmap(fun(_) -> {ok, Val} end, Id).

%%========================================
%% utils to implement new kind of variable
%%========================================
%% @doc get default namespace.
-spec default_namespace() -> atom().
default_namespace() ->
    '$shvar_default_namespace'.

%% @doc extract namespace part from Id.
-spec to_namespace(id()) -> namespace().
to_namespace(#'$shvar_full_id'{namespace = Namespace}) ->
    Namespace;
to_namespace(_Key) ->
    ?MODULE:default_namespace().

%% @doc extract key part from Id.
-spec to_key(id()) -> key().
to_key(#'$shvar_full_id'{key = Key}) ->
    Key;
to_key(Key) ->
    Key.

%% @doc manipulate pool atomically.
-spec run(Fun, namespace()) -> Ret when
        Fun :: fun((Pool) -> {Ret, Pool}),
        Pool :: map(),
        Ret :: any().
run(Fun, Namespace) ->
    case send({run, Fun}, Namespace) of
        {ok, Ret} ->
            Ret;
        {error, {E, R}} ->
            erlang:E(R)
    end.

%% @doc foldmap on the variable. see get/1 and put/2 for usage.
-spec foldmap(fun((val() | undefined) -> {Ret, val()}), id()) -> Ret.
foldmap(Fun, Id) ->
    Key = to_key(Id),
    Namespace = to_namespace(Id),
    RunFun = fun(Pool0) ->
                     Val0 = getter(Key, Pool0),
                     {Ret, Val1} = Fun(Val0),
                     Pool1 = setter(Key, Val1, Pool0),
                     {Ret, Pool1}
             end,
    run(RunFun, Namespace).

%% @doc map on the variable. see shvar_lists:push/2 for usage.
-spec map(fun((val() | undefined) -> val()), id()) -> val().
map(MapFun, Id) ->
    FoldMapFun = fun(Val0) -> Val1 = MapFun(Val0), {Val1, Val1} end,
    foldmap(FoldMapFun, Id).

%% @doc get current variable pool on Namespace (for debug).
-spec get_pool(namespace()) -> pool().
get_pool(Namespace) ->
    shvar:run(fun(Pool0) -> {Pool0, Pool0} end, Namespace).

%%================================================================================
%% internal functions
%%================================================================================
-spec init_impl(atom(), list()) -> {ok, pid()} | {error, already_started}.
init_impl(Namespace, SpawnOpts) ->
    Pid = erlang:spawn_opt(fun worker/0, SpawnOpts),
    try
        true = erlang:register(Namespace, Pid),
        ok = send(init, Namespace),
        {ok, Pid}
    catch
        error:badarg ->
            true = erlang:unlink(Pid),
            Pid ! ?MSG(erlang:self(), exit),
            {error, already_started}
    end.

-spec worker() -> _.
worker() ->
    worker_1(#{}).

-spec worker_1(pool()) -> _.
worker_1(Pool0) ->
    Result =
        receive
            ?MSG(From, init) ->
                ack(ok, From),
                {continue, Pool0};
            ?MSG(From, {run, Fun}) ->
                try
                    {Ret, Pool1} = Fun(Pool0),
                    ack({ok, Ret}, From),
                    {continue, Pool1}
                catch
                    E:R ->
                        ack({error, {E, R}}, From),
                        {continue, Pool0}
                end;
            ?MSG(From, exit) ->
                ack(ok, From),
                done
        end,
    case Result of
        done -> ok;
        {continue, Pool9} ->
            worker_1(Pool9)
    end.

-spec getter(key(), pool()) -> val().
getter(Key, Pool) ->
    maps:get(Key, Pool, undefined).

-spec setter(key(), val(), pool()) -> pool().
setter(Key, Val, Pool) ->
    ?COND(Val =:= undefined, maps:remove(Key, Pool), Pool#{Key => Val}).

-spec send(any(), atom()) -> any().
send(Msg, Namespace) ->
    Pid = erlang:whereis(Namespace),
    _ = erlang:is_pid(Pid) orelse erlang:error({uninitialized_namespace, Namespace}),
    Pid ! ?MSG(erlang:self(), Msg),
    receive
        ?MSG(Pid, {ack, Reply}) ->
            Reply
    end.

-spec ack(any(), pid()) -> _.
ack(Val, Destination) ->
    Destination ! ?MSG(erlang:self(), {ack, Val}).
