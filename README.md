# shvar
Simple and **non**-scalable shared variables implementation in Erlang, mainly for tests.

## Build
    $ ./rebar3 compile

## Brief usage
    $ ./rebar3 shell
    1> shvar:init().
    ok
    2> shvar_counter:incr(hoge).
    1
    3> shvar_counter:incr(hoge).
    2
    4> shvar:get(hoge).
    2
    5> shvar_lists:push(hoge, x).
    [x]
    6> shvar_lists:push(hoge, y).
    [y,x]
    7> shvar_lists:push(hoge, z).
    [z,y,x]
    8> shvar_lists:pop(hoge).
    z
    11> shvar:get(hoge).
    [y,x]
    9> shvar_lists:pop(hoge).
    y
    10> shvar_lists:pop(hoge).
    x
    12> shvar:uninit().
    ok
