-module(server).

-behaviour(application).

-export([start/2, stop/1, main/1, bootstrap/0, test/0, test2/0]).

-record(files, {name, hashes}).
-record(locations, {hash, offset, size}).

bootstrap() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(files, [{attributes, record_info(fields, files)}]),
    mnesia:create_table(locations, [{attributes, record_info(fields, locations)}]).

insert_files(File) ->
    Fun = fun() -> mnesia:write(File) end,
    mnesia:transaction(Fun).

test() ->
    File = #files{name = "russian_dish", hashes = ["hash1", "hash2"]},
    insert_files(File),
    F = fun() ->
           [E] = mnesia:read(files, "russian_dish"),
           io:format("Files ~p\n", [E])
        end,
    mnesia:transaction(F).

test2() ->
    test(),
    File = #files{name = "many_dishes", hashes = ["hash1", "kodujuust"]},
    insert_files(File),
    F = fun() ->
           [E] = mnesia:read(files, "many_dishes"),
           NewHashes = lists:append(E#files.hashes, ["couscous", "toad in a hole"]),
           New = E#files{hashes = NewHashes},
           mnesia:write(New)
        end,
    mnesia:transaction(F),
    Q = fun() ->
           [E] = mnesia:read(files, "many_dishes"),
           io:format("Files ~p\n", [E])
        end,
    mnesia:transaction(Q).

%% insert_file(Name, Hashes) ->

    %% mnesia:transaction(fun () -> mnesia:write(#files) end)

start(_, _) ->
    io:fwrite("Hello World\n"),
    {ok, self()}.

stop(_) ->
    ok.

main(_) ->
    start(none, none).
