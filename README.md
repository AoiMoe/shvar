# shvar
Simple and **non**-scalable shared variables implementation in Erlang, mainly for tests.

## Build
    $ make compile

## Brief usage
    $ make shell
    1> shvar:init().
    ok
    2> shvar_counter:incr(hoge).
    1
    3> shvar_counter:incr(hoge).
    2
    4> shvar:get(hoge).
    2
    5> shvar:reset(hoge).
    ok
    6> shvar_lists:push(x, hoge).
    [x]
    7> shvar_lists:push(y, hoge).
    [y,x]
    8> shvar_lists:push(z, hoge).
    [z,y,x]
    9> shvar_lists:pop(hoge).
    z
    10> shvar:get(hoge).
    [y,x]
    11> shvar_lists:pop(hoge).
    y
    12> shvar_lists:pop(hoge).
    x
    13> shvar:uninit().
    ok
