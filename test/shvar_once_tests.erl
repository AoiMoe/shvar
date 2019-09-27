-module(shvar_once_tests).

-define(tm, shvar_once).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"run once",
       fun() ->
               C = counter_key,
               O = once_key,
               N = 10,
               _ = shvar_counter:new(C),
               Do = fun() -> ?tm:run(fun() -> shvar_counter:incr(C) end, O) end,
               ?assertEqual({done, 1}, Do()),
               [?assertEqual({done, 1}, Do()) || _ <- lists:seq(1, N)],
               ?assertEqual(1, ?tm:wait_for(O)),
               ?assertEqual(1, shvar:get(C)), % ensure to be memoized.
               ok
       end},
      {"exception",
       fun() ->
               C = counter_key,
               O = once_key,
               N = 10,
               E = error,
               R = reason,
               _ = shvar_counter:new(C),
               Do = fun() -> ?tm:run(fun() -> shvar_counter:incr(C), erlang:E(R) end, O) end,
               ?assertException(E, R, Do()),
               ?assertEqual({?tm, {failed, {E, R}}}, shvar:get(O)),
               [?assertException(E, R, Do()) || _ <- lists:seq(1, N)], % ensure that the exception is reproduced on run/2.
               ?assertException(E, R, ?tm:wait_for(O)), % ensure that the exception is reproduced on wait_for/1.
               ?assertEqual(1, shvar:get(C)), % ensure to be memoized even if exception is thrown.
               ok
       end},
      {"concurrency",
       fun() ->
               C = counter_key,
               O = once_key,
               N = 10,
               W = timer:seconds(1),
               _ = shvar_counter:new(C),
               Pid = erlang:self(),
               S = started,
               Do = fun() -> ?tm:run(fun() -> Pid ! S, timer:sleep(W), shvar_counter:incr(C) end, O) end,
               PF = {PidF, _RefF} = erlang:spawn_monitor(fun() -> ?assertEqual({done, 1}, Do()) end),
               receive S -> ok end, % ensure that the first process is started.
               PSs = [erlang:spawn_monitor(
                          fun() ->
                                  ?assertEqual(running, Do()), % the first process will be blocking on timer:sleep/1.
                                  ?assertEqual(1, ?tm:wait_for(O)) % wait for terminaton of the first process.
                          end)
                      || _ <- lists:seq(1, N)],
               Ps = [PF | PSs],
               [?assertNotEqual(undefined, erlang:process_info(P, status)) || {P, _} <- Ps], % all processes are running.
               ?assertEqual(1, ?tm:wait_for(O)),
               ?assertEqual(undefined, erlang:process_info(PidF, status)), % the first process already terminated here.
               [receive {'DOWN', R, process, P, normal} -> ok end || {P, R} <- Ps], % eat all down messages.
               [?assertEqual(undefined, erlang:process_info(P, status)) || {P, _} <- Ps], % all processes terminated.
               receive S -> ok after 0 -> ?assert(true) end, % more started messages are not send.
               ?assertEqual(1, shvar:get(C)), % ensure to be memoized.
               ok
       end}
     ]}.

setup() ->
    {ok, _} = shvar:init().

teardown(_) ->
    shvar:uninit().
