-module(shvar_once).

-compile([no_auto_import]).

-export([
         new/1,
         run/2,
         wait_for/1,
         wait_for/2
        ]).

-include("include/impl.hrl").

-define(now_ms(), erlang:monotonic_time(milli_seconds)). % OTP/18

-type state() :: initial
               | running
               | {done, any()}
               | {failed, exception()}
                 .
-type exception() :: {error | throw | exit, any()}.

%%================================================================================
%% exported functions
%%================================================================================
%% @doc initialize run-once.
-spec new(id()) -> ok.
new(Id) ->
    shvar:set(wrap(initial), Id).

%% @doc run once.
-spec run(fun(() -> Ret), id()) -> {done, Ret} | running.
run(Fun, Id) ->
    FoldMapFun =
        fun(Val0) ->
                case unwrap(Val0) of
                    initial -> {initial, wrap(running)};
                    State -> {State, Val0}
                end
        end,
    case shvar:foldmap(FoldMapFun, Id) of
        initial ->
            try
                Ret = Fun(),
                shvar:set(wrap({done, Ret}), Id),
                {done, Ret}
            catch
                E:R ->
                    shvar:set(wrap({failed, {E, R}}), Id),
                    erlang:E(R)
            end;
        {failed, {E, R}} ->
            erlang:E(R);
        State ->
            State
    end.

%% @equiv wait_for([], Id)
-spec wait_for(id()) -> Ret :: any().
wait_for(Id) ->
    wait_for([], Id).

%% @doc wait for the completion of the previous run/2 call for Id.
-spec wait_for([Opt], id()) -> Ret :: any() when
        Opt :: {interval_ms, IntervalMs :: pos_integer()}
             | {timeout_ms, TimeoutMs :: pos_integer() | infinity}
               .
wait_for(Opts, Id) ->
    IntervalMs = proplists:get_value(interval_ms, Opts, 100),
    DeadlineMs =
        case proplists:get_value(timeout_ms, Opts, infinity) of
            infinity -> infinity;
            TimeoutMs -> ?now_ms() + TimeoutMs
        end,
    wait_for_done(DeadlineMs, IntervalMs, Id).

%%================================================================================
%% internal functions
%%================================================================================
-spec unwrap(val()) -> state().
unwrap(undefined) ->
    initial;
unwrap({?MODULE, X}) ->
    X;
unwrap(_) ->
    erlang:error(unmatch_type).

-spec wrap(state()) -> {?MODULE, state()}.
wrap(S) ->
    {?MODULE, S}.

-spec wait_for_done(DeadlineMs :: integer() | infinity, IntervalMs :: pos_integer(), id()) -> Ret :: any().
wait_for_done(DeadlineMs, IntervalMs, Id) ->
    case DeadlineMs < ?now_ms() of
        true -> erlang:error(timeout);
        false ->
            %% note: any atom (including 'infinity') is bigger than any number.
            case unwrap(shvar:get(Id)) of
                {done, Ret} ->
                    Ret;
                {failed, {E, R}} ->
                    erlang:E(R);
                _ ->
                    timer:sleep(IntervalMs),
                    wait_for_done(DeadlineMs, IntervalMs, Id)
            end
    end.
