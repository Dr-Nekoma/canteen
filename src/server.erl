-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1]).

start(_, _) ->
    io:fwrite("Hello World\n"),
    {ok, self()}.

stop(_) ->
    ok.

main(_) ->
    start(none,none).
