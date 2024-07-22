-module(disk).

-export([keeper/1, main/0, compose/1, bootstrap/0, match_command/1, accept/1]).

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

create_command({write, Filename, Content}) ->
    Hash = crypto:hash(sha256, Content),
    Function = (fun () ->
			case mnesia:read(files, Filename) of
			    [] -> 
				mnesia:write(#files{name = Filename, hashes = [Hash]});
			    [{_,_,Latest}|Rest] ->
				mnesia:write(#files{name = Filename, hashes = [[Hash|Latest],Latest|Rest]})
			end,
			case mnesia:read(locations, Hash) of
			    [] -> case mnesia:read(cursor, Filename) of
				      [] ->
					  Location = #locations{hash = Hash, size = erlang:byte_size(Content), offset = 0},
					  mnesia:write(Location);
				      [{_,_,Offset}] ->
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
    {Hash, Function};

%% TODO: Refactor common parts between this and above
create_command({write, Filename, Content, HashToReplace}) ->
    Hash = crypto:hash(sha256, Content),
    Function = (fun () ->
			case mnesia:read(files, Filename) of
			    [] -> 
				%% TODO: Let it crash, there is no hash to replace in this case
				mnesia:write(#files{name = Filename, hashes = [Hash]});
			    [{_,_,Latest}|Rest] ->
				NewLatest = lists:map(fun (H) -> if H == HashToReplace -> Hash; true -> H end end, Latest),
				Latestest = case lists:member(Hash, NewLatest) of
						true -> NewLatest;
						false -> [Hash|NewLatest] 
					    end,
				mnesia:write(#files{name = Filename, hashes = [Latestest,Latest|Rest]})
			end,
			case mnesia:read(locations, Hash) of
			    [] -> case mnesia:read(cursor, Filename) of
				      [] ->
					  Location = #locations{hash = Hash, size = erlang:byte_size(Content), offset = 0},
					  mnesia:write(Location);
				      [{_,_,Offset}] ->
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
    {Hash, Function}.

compose([]) -> fun () -> nothing end;

compose(Commands) ->
    lists:foldr(fun ({Hash, Function}, {HashAcc, FunAcc}) -> {[Hash|HashAcc], (fun () -> Function(), FunAcc() end)} end, 
		{[], (fun () -> ok end)},
		lists:map(fun (X) -> create_command(X) end, Commands)).


match_command(<<Operation:1/binary,
		BinFileNameLength:4/binary,
		BinContentLength:4/binary,
		Rest/binary>>) ->
    FileNameLength = binary:decode_unsigned(BinFileNameLength, big),
    ContentLength = binary:decode_unsigned(BinContentLength, big),
    <<FileName:FileNameLength/binary, Content:ContentLength/binary, _/binary>> = Rest,
    case Operation of
	<<"W">> -> 
	    {Hashes, Operations} = compose([{write, erlang:binary_to_list(FileName), Content}]),
	    Res = mnesia:transaction(Operations),
	    io:format("~p\n~p\n", [Res, Hashes]),
	    {Hashes, Res};
	_ -> nope
    end.


accept(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}, {packet, 0}, {reuseaddr, true}]),
    io:format("Listening: ~p~n", [Port]),
    server(Socket).

server(Socket) ->
    {ok, Connection} = gen_tcp:accept(Socket),
    Handler = spawn(fun () -> loop(Connection, Socket) end),
    gen_tcp:controlling_process(Connection, Handler),
    server(Socket).

loop(Connection, Socket) ->
    receive
	% Bound var in patten as intended.
        {tcp, Connection, Data} ->
	    {Hashes, _} = match_command(Data),
	    io:format("Hashes: ~p\n", [Hashes]),
	    gen_tcp:send(Socket, "Abc");
	    %% case gen_tcp:send(Socket, "Abc") of
            %%     {error, timeout} ->
            %%         io:format("Send timeout, closing!~n",
            %%                   []);
            %%     {error, OtherSendError} ->
            %%         io:format("Some other error on socket (~p), closing",
            %%                   [OtherSendError]);
            %%     ok -> loop(Connection, Socket)
            %% end;
	{tcp_closed, Connection} -> connection_closed
    end.

main() ->
    %% Files :: Filename => Hash list list
    %% FilesProc = spawn(?MODULE, keeper, [nothing]),
    %% Locations :: Hash => {size : int; offset: int}
    %% LocationsProc = spawn(?MODULE, keeper, [nothing]),
    %% LocationsProc = spawn(?MODULE, server, []),
    {Hashes, Operations} = compose([{write, "blacklist", <<"catboys">>}]),
    Res = mnesia:transaction(Operations),
    {_, NewOperations} = compose([{write, "blacklist", <<"maidboys">>, hd(Hashes)}]),
    Res2 = mnesia:transaction(NewOperations),
    io:format("Stage 1: ~p\nStage 2: ~p\n", [Res, Res2]),
    Read = (fun () -> 
		    case mnesia:read(files, "blacklist") of
			[] -> [];
			[Content] -> Content
		    end
	    end),
    Res3 = mnesia:transaction(Read),
    io:format("Stage 3: ~p\n", [Res3]),
    %% FileSize = binary:encode_unsigned(3),
    %% ContentSize = binary:encode_unsigned(4),
    %% Content = binary:encode_unsigned(1),
    %% Message2 = <<"W",FileSize,ContentSize,0,0,0,0,0,0,0,0,"abc",Content>>,
    ok.

%% [[3],
%%  [3,4,1],
%%  [3,2,1]
%%  [2,1],
%%  [1],
%%  []]
