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

%%================================================================================
%% exported functions
%%================================================================================
%% @doc initialize run-once.
-spec new(id()) -> ok.
new(Id) ->
    shvar:set({?MODULE, initial}, Id).

%% @doc run once.
-spec run(fun(() -> Ret), id()) -> {done, Ret} | running.
run(Fun, Id) ->
    FoldMapFun =
        fun(Val0) ->
                case ensure(Val0) of
                    {Tag, initial} -> {initial, {Tag, running}};
                    {_Tag, State} -> {State, Val0}
                end
        end,
    case shvar:foldmap(FoldMapFun, Id) of
        initial ->
            try
                Ret = Fun(),
                shvar:set({?MODULE, {done, Ret}}, Id),
                {done, Ret}
            catch
                E:R ->
                    shvar:set({?MODULE, initial}, Id),
                    erlang:E(R)
            end;
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
-spec ensure(any()) -> {?MODULE, initial | running | {done, any()}}.
ensure(undefined) ->
    {?MODULE, initial};
ensure({?MODULE, _} = X) ->
    X;
ensure(_) ->
    erlang:error(unmatch_type).

-spec wait_for_done(DeadlineMs :: integer() | infinity, IntervalMs :: pos_integer(), id()) -> Ret :: any().
wait_for_done(DeadlineMs, IntervalMs, Id) ->
    case DeadlineMs < ?now_ms() of
        true -> erlang:error(timeout);
        false ->
            %% note: any atom (including 'infinity') is bigger than any number.
            case shvar:get(Id) of
                {?MODULE, {done, Ret}} ->
                    Ret;
                X ->
                    case X of
                        {?MODULE, _} -> ok;
                        undefined -> ok;
                        _ -> erlang:error(unmatch_type)
                    end,
                    timer:sleep(IntervalMs),
                    wait_for_done(DeadlineMs, IntervalMs, Id)
            end
    end.
