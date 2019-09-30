-module(shvar_counter).

-compile([no_auto_import]).

-export([
         new/1,
         get/1,
         incr/1,
         incr/2,
         decr/1,
         decr/2
        ]).

-include("include/impl.hrl").

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new counter into the variable specified by Id.
-spec new(id()) -> ok.
new(Id) ->
    shvar:set(0, Id).

%% @doc get counter value (with 'ensure').
-spec get(id()) -> integer().
get(Id) ->
    ensure(shvar:get(Id)).

%% @equiv incr(sync, Id)
-spec incr(id()) -> integer().
incr(Id) ->
    incr(sync, Id).

%% @doc increment counter.
-spec incr(sync, id()) -> integer();
          (async, id()) -> ok.
incr(Synchronousness, Id) ->
    shvar:map(fun(Val0) -> ensure(Val0)+1 end, Synchronousness, Id).

%% @equiv decr(sync, Id)
-spec decr(id()) -> integer().
decr(Id) ->
    decr(sync, Id).

%% @doc decrement counter.
-spec decr(sync, id()) -> integer();
          (async, id()) -> ok.
decr(Synchronousness, Id) ->
    shvar:map(fun(Val0) -> ensure(Val0)-1 end, Synchronousness, Id).

%%================================================================================
%% internal functions
%%================================================================================
-spec ensure(any()) -> number().
ensure(Val) when erlang:is_integer(Val) ->
    Val;
ensure(undefined) ->
    0;
ensure(_) ->
    erlang:error(unmatch_type).
