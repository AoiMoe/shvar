-module(shvar__pool).

-compile([no_auto_import]).

-export([
         new/0,
         get/2,
         put/3,
         keys/1,
         values/1,
         to_list/1
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
