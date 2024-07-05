-module(disk).

-export([keeper/1, main/0]).

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

%% compose([{write, Filename, Content} | Tail]) ->
%%     Hash = crypto:hash(sha256, Content),
%%     FilesCommand = (fun (HashTable) ->
%%                             Default = [],
%%                             [C|Cs] = maps:get(Filename, HashTable, Default),
%%                             maps:put(Filename, [[Hash|C],C|Cs], HashTable) end),
%%     %% Write to mnesia (our WAL) here the commit to effectively change the file later
%%     LocationsCommand = (fun (HashTable) ->
%%                             case maps:get(Hash, HashTable, garbage) of
%%                                 garbage -> gotta_write;
%%                                 #{size := _, offset := _} -> ok
%%                             end,
%%                             maps:put(Filename, [[Hash|C],C|Cs], HashTable) end),
%%     [{FilesCommand, LocationsCommand} | compose(Tail)].

main() ->
    %% Files :: Filename => Hash list list
    FilesProc = spawn(?MODULE, keeper, [nothing]),
    %% Locations :: Hash => {size : int; offset: int}
    LocationsProc = spawn(?MODULE, keeper, [nothing]),
    %% compose([{write, "blacklist", <<"catboys">>},
             %% {write, "blacklist", <<"maidboys">>, <<"123">>}]),
    ok.

%% [[3],
%%  [3,4,1],
%%  [3,2,1]
%%  [2,1],
%%  [1],
%%  []]
