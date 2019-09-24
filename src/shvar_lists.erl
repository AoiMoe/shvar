-module(shvar_lists).

-compile([no_auto_import]).

-export([
         new/1,
         map_whole/2,
         foldmap_whole/2,
         push/2,
         pop/1
        ]).

-include("include/impl.hrl").

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new list into the variable specified by Id.
-spec new(id()) -> ok.
new(Id) ->
    shvar:set([], Id).

%% @doc map for whole list (not for each elements).
-spec map_whole(fun(([any()]) -> [any()]), id()) -> [any()].
map_whole(Fun, Id) ->
    shvar:map(fun(Val0) -> Fun(ensure(Val0)) end, Id).

%% @doc foldmap for whole list (not for each elements).
-spec foldmap_whole(fun(([any()]) -> {Ret, [any()]}), id()) -> Ret.
foldmap_whole(Fun, Id) ->
    FoldMapFun =
        fun(Val0) ->
                Fun(ensure(Val0))
        end,
    shvar:foldmap(FoldMapFun, Id).

%% @doc push element to list.
-spec push(any(), id()) -> [any()].
push(Elem, Id) ->
    map_whole(fun(List0) -> [Elem | List0] end, Id).

%% @doc pop element from list.
-spec pop(id()) -> any(). % | error(badarg)
pop(Id) ->
    FoldMapFun =
        fun([H|T]) ->
                {H, T};
           (_) ->
                erlang:error(badarg)
        end,
    foldmap_whole(FoldMapFun, Id).

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
