-module(shvar_counter).

-compile([no_auto_import]).

-export([
         new/1,
         incr/1,
         decr/1
        ]).

-include("include/impl.hrl").

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new counter into the variable specified by Id.
-spec new(id()) -> ok.
new(Id) ->
    shvar:set(0, Id).

%% @doc increment counter.
-spec incr(id()) -> integer().
incr(Id) ->
    shvar:map(fun(Val0) -> ensure(Val0)+1 end, Id).

%% @doc decrement counter.
-spec decr(id()) -> integer().
decr(Id) ->
    shvar:map(fun(Val0) -> ensure(Val0)-1 end, Id).

%%================================================================================
%% internal functions
%%================================================================================
-spec ensure(any()) -> number().
ensure(Val) when erlang:is_integer(Val) ->
    Val;
ensure(_) ->
    0.
