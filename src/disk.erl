-module(disk).

-export([keeper/1, main/0, compose/1, bootstrap/0]).

-record(files, {name, hashes}).
-record(locations, {hash, offset, size}).
-record(cursor, {filename, offset}).

bootstrap() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(files, [{attributes, record_info(fields, files)}]),
    mnesia:create_table(cursor, [{attributes, record_info(fields, cursor)}]),
    mnesia:create_table(locations, [{attributes, record_info(fields, locations)}]).

keeper(HashTable, Pid, updated) ->
    Pid ! ok,
    keeper(HashTable);

keeper(HashTable, Pid, {error, Msg}) ->
    Pid ! {error, Msg},
    keeper(HashTable).

keeper(nothing) ->
    keeper(maps:new());

keeper(HashTable) ->
    receive 
        {Pid, update, Function} -> 
            try apply(Function, [HashTable])
            of NewHashTable -> keeper(NewHashTable, Pid, updated)
            catch Error -> keeper(HashTable, Pid, {error, Error}) end;
        {Pid, get} -> Pid ! {ok, HashTable}, keeper(HashTable);
        stop -> ok
    end.

compose([{write, Filename, Content} | Tail]) ->
    Hash = crypto:hash(sha256, Content),
    Command = 
	(fun () ->
		 case mnesia:read(files, Filename) of
		     [] -> 
			 mnesia:write(#files{name = Filename, hashes = [Hash]});
		     [Latest|Rest] ->
			 mnesia:write(#files{name = Filename, hashes = [[Hash|Latest],Latest|Rest]})
		 end,
		 case mnesia:read(locations, Hash) of
		     [] -> case mnesia:read(cursor, Filename) of
			       [] ->
				   Location = #locations{hash = Hash, size = erlang:byte_size(Content), offset = 0},
				   mnesia:write(Location);
			       [Offset] ->
				   Location = #locations{hash = Hash, size = erlang:byte_size(Content), offset = Offset},
				   mnesia:write(Location)
			   end;
		     [_] -> already_registered
		 end,
		 case mnesia:read(cursor, Filename) of
		     [] -> mnesia:write(#cursor{filename = Filename, offset = erlang:byte_size(Content)});
		     [Cursor] -> mnesia:write(#cursor{filename = Filename, offset = Cursor#cursor.offset + erlang:byte_size(Content)})
		 end
	 end),
    [Command | compose(Tail)];

compose([]) -> [].

main() ->
    %% Files :: Filename => Hash list list
    %% FilesProc = spawn(?MODULE, keeper, [nothing]),
    %% Locations :: Hash => {size : int; offset: int}
    %% LocationsProc = spawn(?MODULE, keeper, [nothing]),
    [Commands] = compose([{write, "blacklist", <<"catboys">>}]),
             %% {write, "blacklist", <<"maidboys">>, <<"123">>}]),
    Res = mnesia:transaction(Commands),
    io:format("~p\n", [Res]),
    ok.

%% [[3],
%%  [3,4,1],
%%  [3,2,1]
%%  [2,1],
%%  [1],
%%  []]
