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
         set/3,
         reset/1,
         keys/1,
         values/1,
         to_list/1,
         default_namespace/0,
         to_namespace/1,
         to_key/1,
         run/2,
         run/3,
         foldmap/2,
         foldmap/3,
         map/2,
         map/3,
         get_from_pool/2,
         put_into_pool/3,
         get_pool/1
        ]).

-export_type([
              namespace/0,
              key/0,
              val/0,
              id/1,
              id/0,
              full_id/1,
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

-opaque full_id(_Key) :: #'$shvar_full_id'{}.
-opaque full_id() :: full_id(key()).

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
    ok = send(exit, sync, Namespace).

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

%% @equiv set(Val, sync, Id)
%% @doc set Val into the variable specified by Id -- HEADS UP: be careful of argument order.
-spec set(val(), id()) -> ok.
set(Val, Id) ->
    set(Val, sync, Id).

%% @doc set Val into the variable specified by Id.
-spec set(val(), sync | async, id()) -> ok.
set(Val, Synchronousness, Id) ->
    ok = foldmap(fun(_) -> {ok, Val} end, Synchronousness, Id).

%% @doc reset the variable specified by Id.
-spec reset(id()) -> ok.
reset(Id) ->
    ok = set(undefined, Id).

%% @doc get all keys.
-spec keys(namespace()) -> [key()].
keys(Namespace) ->
    run(fun(Pool) -> {maps:keys(Pool), Pool} end, Namespace).

%% @doc get all values.
-spec values(namespace()) -> [val()].
values(Namespace) ->
    run(fun(Pool) -> {maps:values(Pool), Pool} end, Namespace).

%% @doc convert to list.
-spec to_list(namespace()) -> [{key(), val()}].
to_list(Namespace) ->
    maps:to_list(get_pool(Namespace)).

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

%% @equiv run(Fun, sync, Namespace)
-spec run(fun((pool()) -> {Ret, pool()}), namespace()) -> Ret.
run(Fun, Namespace) ->
    run(Fun, sync, Namespace).

%% @doc manipulate pool atomically.
-spec run(fun((pool()) -> {Ret, pool()}), sync, namespace()) -> Ret;
         (fun((pool()) -> {_, pool()}), async, namespace()) -> ok.
run(Fun, Synchronousness, Namespace) ->
    case send({run, Fun}, Synchronousness, Namespace) of
        ok ->
            ok;
        {ok, Ret} ->
            Ret;
        {error, {E, R}} ->
            erlang:E(R)
    end.

%% @equiv foldmap(Fun, sync, Id)
-spec foldmap(fun((val() | undefined) -> {Ret, val()}), id()) -> Ret.
foldmap(FoldFun, Id) ->
    foldmap(FoldFun, sync, Id).

%% @doc foldmap operation on the variable specified by Id.
%% see get/1 and set/2 for usage.<br />
%% HEADS UP: this is not `mapfold', and be careful of the order of the return tuple of FoldFun.
-spec foldmap(fun((val() | undefined) -> {Ret, val()}), sync, id()) -> Ret;
             (fun((val() | undefined) -> {_, val()}), async, id()) -> ok.
foldmap(FoldFun, Synchronousness, Id) ->
    Key = to_key(Id),
    Namespace = to_namespace(Id),
    RunFun = fun(Pool0) ->
                     Val0 = get_from_pool(Key, Pool0),
                     {Ret, Val1} = FoldFun(Val0),
                     Pool1 = put_into_pool(Key, Val1, Pool0),
                     {Ret, Pool1}
             end,
    run(RunFun, Synchronousness, Namespace).

%% @equiv map(MapFun, sync, Id)
-spec map(fun((val() | undefined) -> val()), id()) -> val().
map(MapFun, Id) ->
    map(MapFun, sync, Id).

%% @doc map operation on the variable specified by Id. see shvar_lists:push/2 for usage.
-spec map(fun((val() | undefined) -> val()), sync, id()) -> val();
         (fun((val() | undefined) -> val()), async, id()) -> ok.
map(MapFun, Synchronousness, Id) ->
    FoldMapFun = fun(Val0) -> Val1 = MapFun(Val0), {Val1, Val1} end,
    foldmap(FoldMapFun, Synchronousness, Id).

-spec get_from_pool(key(), pool()) -> val().
get_from_pool(Key, Pool) ->
    maps:get(Key, Pool, undefined).

-spec put_into_pool(key(), val(), pool()) -> pool().
put_into_pool(Key, Val, Pool) ->
    ?COND(Val =:= undefined, maps:remove(Key, Pool), Pool#{Key => Val}).

%% @doc get current variable pool on Namespace (for debug).
-spec get_pool(namespace()) -> pool().
get_pool(Namespace) ->
    shvar:run(fun(Pool0) -> {Pool0, Pool0} end, Namespace).

%%================================================================================
%% internal functions
%%================================================================================
-spec init_impl(atom(), list()) -> {ok, pid()} | {error, already_started}.
init_impl(Namespace, SpawnOpts) ->
    case erlang:whereis(Namespace) of
        undefined ->
            Pid = erlang:spawn_opt(fun worker/0, SpawnOpts),
            try
                true = erlang:register(Namespace, Pid),
                ok = send(init, sync, Namespace),
                {ok, Pid}
            catch
                error:badarg ->
                    %% some race condition exists.
                    true = erlang:unlink(Pid),
                    send(exit, async, Pid),
                    init_impl(Namespace, SpawnOpts)
            end;
        _ ->
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

-spec send(any(), sync, namespace() | pid()) -> any();
          (any(), async, namespace() | pid()) -> ok.
send(Msg, Syncness, Namespace) when erlang:is_atom(Namespace) ->
    Pid = erlang:whereis(Namespace),
    Pid =:= undefined andalso erlang:error({uninitialized_namespace, Namespace}),
    send(Msg, Syncness, Pid);
send(Msg, async, Pid) ->
    Pid ! ?MSG(undefined, Msg),
    ok;
send(Msg, sync, Pid) ->
    Pid ! ?MSG(erlang:self(), Msg),
    receive
        ?MSG(Pid, {ack, Reply}) ->
            Reply
    end.

-spec ack(any(), pid() | undefined) -> _.
ack(_Val, undefined) ->
    ok;
ack(Val, Destination) ->
    Destination ! ?MSG(erlang:self(), {ack, Val}).
