-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1]).

start(_, _) ->
    Connection = database_connect(),
    Pid = spawn(?MODULE, handle_message, [Connection]),
    erlang:register(lyceum_server, Pid),
    {ok, Pid}.

stop(_) ->
    ok.

main(_) ->
    start(none,none).
