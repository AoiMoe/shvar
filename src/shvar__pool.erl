-module(shvar__pool).

-compile([no_auto_import]).

-export([
         new/0,
         get/2,
         put/3,
         keys/1,
         values/1,
         to_list/1,
         foldmap/3,
         map/2
        ]).
-export_type([
              pool/0
             ]).


-include("include/impl.hrl").

-opaque pool() :: map().

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new pool.
-spec new() -> pool().
new() ->
    #{}.

%% @doc get value associated with Key from Pool.
-spec get(key(), pool()) -> val().
get(Key, Pool) ->
    maps:get(Key, Pool, undefined).

%% @doc put Val associated with Key into Pool.
-spec put(key(), val(), pool()) -> pool().
put(Key, Val, Pool) ->
    ?COND(Val =:= undefined, maps:remove(Key, Pool), Pool#{Key => Val}).

%% @doc get keys from Pool.
-spec keys(pool()) -> [key()].
keys(Pool) ->
    maps:keys(Pool).

%% @doc get values from Pool.
-spec values(pool()) -> [val()].
values(Pool) ->
    maps:values(Pool).

%% @doc convert Pool to assoc list.
-spec to_list(pool()) -> [{key(), val()}].
to_list(Pool) ->
    maps:to_list(Pool).

%% @doc foldmap operation over Pool.
-spec foldmap(fun((key(), val(), _Acc1) -> {_Acc2, val() | undefined}), _Acc0, pool()) -> {_Acc3, pool()}.
foldmap(FoldMapFun, Acc0, Pool) ->
    {Acc3, Pool3} =
        maps:fold(
            fun(Key, Val0, {Acc1, Pool1}) ->
                    case FoldMapFun(Key, Val0, Acc1) of
                        {Acc2, undefined} ->
                            {Acc2, Pool1};
                        {Acc2, Val1} ->
                            {Acc2, [{Key, Val1} | Pool1]}
                    end
            end, {Acc0, []}, Pool),
    {Acc3, maps:from_list(Pool3)}.

%% @doc map operation over Pool.
-spec map(fun((key(), val()) -> val() | undefined), pool()) -> pool().
map(MapFun, Pool) ->
    Pool2 =
        maps:fold(
            fun(Key, Val0, Pool1) ->
                    case MapFun(Key, Val0) of
                        undefined -> Pool1;
                        Val1 -> [{Key, Val1} | Pool1]
                    end
            end, [], Pool),
    maps:from_list(Pool2).
