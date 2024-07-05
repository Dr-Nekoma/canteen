-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, bootstrap/0]).

-record(files, {name, hashes}).
-record(locations, {hash, offset, size}).

bootstrap() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(files, [{attributes, record_info(fields, files)}]),
    mnesia:create_table(locations, [{attributes, record_info(fields, locations)}]).

%% insert_file(Name, Hashes) ->
    %% mnesia:transaction(fun () -> mnesia:write(#files) end)

start(_, _) ->
    io:fwrite("Hello World\n"),
    {ok, self()}.

stop(_) ->
    ok.

main(_) ->
    start(none,none).
