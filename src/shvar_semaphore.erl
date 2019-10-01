-module(shvar_semaphore).

-compile([no_auto_import]).

-export([
         new/1,
         acquire/1,
         release/1,
         guard/2
        ]).

-export_type([
              counting_semaphore_key/0,
              binary_semaphore_key/0,
              semaphore_key/0,
              semaphore_id/0
             ]).

-include("include/impl.hrl").

-type counting_semaphore_key() :: {key(), InitialCount::pos_integer()}.
-type binary_semaphore_key() :: key().  % except for {key(), integer()}.
-type semaphore_key() :: counting_semaphore_key() | binary_semaphore_key().
-type semaphore_id() :: id(semaphore_key()).

%%================================================================================
%% exported functions
%%================================================================================
%% @doc new semaphore into the variable specified by Id.
-spec new(semaphore_id()) -> ok.
new(Id) ->
    shvar:set(wrap(id_to_initial_count(Id)), Id).

%% @doc acquire semaphore.
-spec acquire(semaphore_id()) -> boolean().
acquire(Id) ->
    InitialCount = id_to_initial_count(Id),
    FoldMapFun =
        fun(Val0) ->
                case unwrap(Val0, InitialCount) of
                    0 -> {false, Val0};
                    X -> {true, wrap(X-1)}
                end
        end,
    shvar:foldmap(FoldMapFun, Id).

%% @doc release semaphore.
-spec release(semaphore_id()) -> ok.
release(Id) ->
    InitialCount = id_to_initial_count(Id),
    FoldMapFun =
        fun(Val0) ->
                case unwrap(Val0, InitialCount) of
                    X when X >= InitialCount -> erlang:error(unmatch_release);
                    X -> {ok, wrap(X+1)}
                end
        end,
    shvar:foldmap(FoldMapFun, Id).

%% @doc call function guarded by semaphore.
-spec guard(fun(() -> Ret), semaphore_id()) -> false | {true, Ret}.
guard(Fun, Id) ->
    case acquire(Id) of
        true ->
            try
                {true, Fun()}
            after
                release(Id)
            end;
        false ->
            false
    end.

%%================================================================================
%% internal functions
%%================================================================================
-spec unwrap(any(), pos_integer()) -> non_neg_integer().
unwrap({?MODULE, Val}, _) ->
    Val;
unwrap(undefined, InitialCount) ->
    InitialCount;
unwrap(_, _) ->
    erlang:error(unmatch_type).

-spec wrap(non_neg_integer()) -> {?MODULE, non_neg_integer()}.
wrap(Val) ->
    {?MODULE, Val}.

-spec id_to_initial_count(semaphore_id()) -> pos_integer().
id_to_initial_count(Id) ->
    case shvar:to_key(Id) of
        {_, InitialCount} when erlang:is_integer(InitialCount) ->
            if InitialCount =< 0 ->
                    erlang:error(illegal_key);
               true ->
                    InitialCount
            end;
        _ ->
            1
    end.
