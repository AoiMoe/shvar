-module(shvar_semaphore).

-compile([no_auto_import]).

-export([
         new/2,
         new/1,
         acquire/2,
         acquire/1,
         release/2,
         release/1,
         guard/3,
         guard/2
        ]).

-include("include/impl.hrl").

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new semaphore into the variable specified by Id.
-spec new(id(), pos_integer()) -> ok.
new(Id, InitialCount) ->
    shvar:set({{?MODULE, InitialCount}, InitialCount}, Id).

%% @equiv new(Id, 1)
-spec new(id()) -> ok.
new(Id) ->
    new(Id, 1).

%% @doc acquire semaphore.
-spec acquire(id(), pos_integer()) -> boolean().
acquire(Id, InitialCount) ->
    FoldMapFun =
        fun(Val0) ->
                case ensure(Val0, InitialCount) of
                    {_, 0} -> {false, Val0};
                    {Tag, X} -> {true, {Tag, X-1}}
                end
        end,
    shvar:foldmap(FoldMapFun, Id).

%% @equiv acquire(Id, 1)
-spec acquire(id()) -> boolean().
acquire(Id) ->
    acquire(Id, 1).

%% @doc release semaphore.
-spec release(id(), pos_integer()) -> ok.
release(Id, InitialCount) ->
    FoldMapFun =
        fun(Val0) ->
                case ensure(Val0, InitialCount) of
                    {{_, InitialCount}, Count} when Count >= InitialCount -> erlang:error(unmatch_release);
                    {Tag, X} -> {ok, {Tag, X+1}}
                end
        end,
    shvar:foldmap(FoldMapFun, Id).

%% @equiv release(Id, 1)
-spec release(id()) -> ok.
release(Id) ->
    release(Id, 1).

%% @doc call function guarded by semaphore.
-spec guard(fun(() -> Ret), id(), pos_integer()) -> false | {true, Ret}.
guard(Fun, Id, InitialCount) ->
    case acquire(Id, InitialCount) of
        true ->
            try
                {true, Fun()}
            after
                release(Id, InitialCount)
            end;
        false ->
            false
    end.

%% @equiv guard(Id, 1, Fun)
-spec guard(fun(() -> Ret), id()) -> false | {true, Ret}.
guard(Fun, Id) ->
    guard(Fun, Id, 1).

%%================================================================================
%% internal functions
%%================================================================================
-spec ensure(any(), pos_integer()) -> {{?MODULE, pos_integer()}, non_neg_integer()}.
ensure({{?MODULE, InitialCount} = Tag, Val}, InitialCount) ->
    {Tag, Val};
ensure({{?MODULE, _}, _}, _) ->
    %% error if already stored value is semaphore but mismatching initial count.
    erlang:error(unmatch_resource_count);
ensure(undefined, InitialCount) ->
    {{?MODULE, InitialCount}, InitialCount};
ensure(_, _) ->
    erlang:error(unmatch_type).
