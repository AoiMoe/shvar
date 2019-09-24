-module(shvar_lists).

-compile([no_auto_import]).

-export([
         new/1,
         map_whole/2,
         map_whole/3,
         foldmap_whole/2,
         foldmap_whole/3,
         push/2,
         push/3,
         pop/1,
         pop/2
        ]).

-include("include/impl.hrl").

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new list into the variable specified by Id.
-spec new(id()) -> ok.
new(Id) ->
    shvar:set([], Id).

%% @equiv map_whole(Fun, sync, Id)
-spec map_whole(fun(([any()]) -> [any()]), id()) -> [any()].
map_whole(Fun, Id) ->
    map_whole(Fun, sync, Id).

%% @doc map for whole list (not for each elements).
-spec map_whole(fun(([any()]) -> [any()]), sync, id()) -> [any()];
               (fun(([any()]) -> [any()]), async, id()) -> ok.
map_whole(Fun, Synchronousness, Id) ->
    shvar:map(fun(Val0) -> Fun(ensure(Val0)) end, Synchronousness, Id).

%% @equiv foldmap_whole(Fun, sync, Id)
-spec foldmap_whole(fun(([any()]) -> {Ret, [any()]}), id()) -> Ret.
foldmap_whole(Fun, Id) ->
    foldmap_whole(Fun, sync, Id).

%% @doc foldmap for whole list (not for each elements).
-spec foldmap_whole(fun(([any()]) -> {Ret, [any()]}), sync, id()) -> Ret;
                   (fun(([any()]) -> {_, [any()]}), async, id()) -> ok.
foldmap_whole(Fun, Synchronousness, Id) ->
    FoldMapFun =
        fun(Val0) ->
                Fun(ensure(Val0))
        end,
    shvar:foldmap(FoldMapFun, Synchronousness, Id).

%% @equiv push(Elem, sync, Id)
-spec push(any(), id()) -> [any()].
push(Elem, Id) ->
    push(Elem, sync, Id).

%% @doc push element to list.
-spec push(any(), sync, id()) -> [any()];
          (any(), async, id()) -> ok.
push(Elem, Synchronousness, Id) ->
    map_whole(fun(List0) -> [Elem | List0] end, Synchronousness, Id).

%% @equiv pop(sync, Id)
-spec pop(id()) -> any(). % | error(badarg)
pop(Id) ->
    pop(sync, Id).

%% @doc pop element from list.
-spec pop(sync, id()) -> any(); % | error(badarg)
         (async, id()) -> ok.
pop(Synchronousness, Id) ->
    FoldMapFun =
        fun([H|T]) ->
                {H, T};
           (_) ->
                erlang:error(badarg)
        end,
    foldmap_whole(FoldMapFun, Synchronousness, Id).

%%================================================================================
%% internal functions
%%================================================================================
-spec ensure(val()) -> [any()].
ensure(Val) when erlang:is_list(Val) ->
    Val;
ensure(undefined) ->
    [];
ensure(_) ->
    erlang:error(unmatch_type).
