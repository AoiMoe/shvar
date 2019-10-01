-module(shvar_semaphore_tests).

-define(tm, shvar_semaphore).

-include_lib("eunit/include/eunit.hrl").

binary_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"cannot acquire twice, and cannot release twice",
       fun() ->
               K = sem,
               ?assertEqual(true, ?tm:acquire(K)),
               ?assertEqual(false, ?tm:acquire(K)),
               ?assertEqual(ok, ?tm:release(K)),
               ?assertError(unmatch_release, ?tm:release(K)),
               %% keep internal integrity after unmatch_release is caused.
               ?assertEqual(true, ?tm:acquire(K)),
               ?assertEqual(false, ?tm:acquire(K)),
               ok
       end},
      {"type checking",
       fun() ->
               K = sem,
               shvar:set(0, K),
               ?assertError(unmatch_type, ?tm:acquire(K)),
               ?tm:new(K),
               ?assertEqual(true, ?tm:acquire(K)),
               ok
       end},
      {"guard",
       fun() ->
               K = sem,
               F = fun() -> hoge end,
               %% successful case
               ?assertEqual({true, hoge}, ?tm:guard(F, K)),
               %% cannot acquire case
               ?assertEqual(true, ?tm:acquire(K)),
               ?assertEqual(false, ?tm:guard(F, K)),
               ?assertEqual(ok, ?tm:release(K)),
               %% something broken case
               ?assertError(unmatch_release, ?tm:guard(fun() -> ?tm:release(K) end, K)),
               %% but, success again (keep internal integrity)
               ?assertEqual({true, hoge}, ?tm:guard(F, K)),
               ?assertEqual(true, ?tm:acquire(K)),
               ?assertEqual(false, ?tm:guard(F, K)),
               ok
       end}
     ]}.

counting_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [
      {"cannot acquire over 10 times, and cannot release over 10 times",
       fun() ->
               C = 10,
               K = {sem, C},
               [?assertEqual(true, ?tm:acquire(K)) || _ <- lists:seq(1, C)],
               ?assertEqual(false, ?tm:acquire(K)),
               [?assertEqual(ok, ?tm:release(K)) || _ <- lists:seq(1, C)],
               ?assertError(unmatch_release, ?tm:release(K)),
               %% keep internal integrity after unmatch_release is caused.
               [?assertEqual(true, ?tm:acquire(K)) || _ <- lists:seq(1, C)],
               ?assertEqual(false, ?tm:acquire(K)),
               ok
       end},
      {"type checking",
       fun() ->
               C = 10,
               K = {sem, C},
               shvar:set(0, K),
               ?assertError(unmatch_type, ?tm:acquire(K)),
               ?tm:new(K),
               [?assertEqual(true, ?tm:acquire(K)) || _ <- lists:seq(1, C)],
               ok
       end},
      {"guard",
       fun() ->
               C = 10,
               K = {sem, C},
               F = fun() -> hoge end,
               %% successful case
               [?assertEqual(true, ?tm:acquire(K)) || _ <- lists:seq(1, C-1)],
               ?assertEqual({true, hoge}, ?tm:guard(F, K)),
               %% cannot acquire case
               ?assertEqual(true, ?tm:acquire(K)),
               ?assertEqual(false, ?tm:guard(F, K)),
               [?assertEqual(ok, ?tm:release(K)) || _ <- lists:seq(1, C)],
               %% something broken case
               ?assertError(unmatch_release, ?tm:guard(fun() -> ?tm:release(K) end, K)),
               %% but, success again (keep internal integrity)
               [?assertEqual(true, ?tm:acquire(K)) || _ <- lists:seq(1, C-1)],
               ?assertEqual({true, hoge}, ?tm:guard(F, K)),
               ?assertEqual(true, ?tm:acquire(K)),
               ?assertEqual(false, ?tm:guard(F, K)),
               ok
       end}
     ]}.

setup() ->
    {ok, _} = shvar:init().

teardown(_) ->
    shvar:uninit().
