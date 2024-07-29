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
            try apply(Function, [HashTable]) of
                NewHashTable ->
                    keeper(NewHashTable, Pid, updated)
            catch
                Error ->
                    keeper(HashTable, Pid, {error, Error})
            end;
        {Pid, get} ->
            Pid ! {ok, HashTable},
            keeper(HashTable);
        stop ->
            ok
    end.

create_command({write, Filename, Content}) ->
    Hash = crypto:hash(sha256, Content),
    Function =
        fun() ->
           case mnesia:read(files, Filename) of
               [] ->
                   mnesia:write(#files{name = Filename, hashes = [Hash]});
               [{_, _, Latest} | Rest] ->
                   mnesia:write(#files{name = Filename, hashes = [[Hash | Latest], Latest | Rest]})
           end,
           case mnesia:read(locations, Hash) of
               [] ->
                   case mnesia:read(cursor, Filename) of
                       [] ->
                           Location =
                               #locations{hash = Hash,
                                          size = erlang:byte_size(Content),
                                          offset = 0},
                           mnesia:write(Location);
                       [{_, _, Offset}] ->
                           Location =
                               #locations{hash = Hash,
                                          size = erlang:byte_size(Content),
                                          offset = Offset},
                           mnesia:write(Location)
                   end;
               [_] ->
                   already_registered
           end,
           case mnesia:read(cursor, Filename) of
               [] ->
                   mnesia:write(#cursor{filename = Filename, offset = erlang:byte_size(Content)});
               [Cursor] ->
                   mnesia:write(#cursor{filename = Filename,
                                        offset = Cursor#cursor.offset + erlang:byte_size(Content)})
           end
        end,
    {Hash, Function};
%% TODO: Refactor common parts between this and above
create_command({write, Filename, Content, HashToReplace}) ->
    Hash = crypto:hash(sha256, Content),
    Function =
        fun() ->
           case mnesia:read(files, Filename) of
               [] ->
                   %% TODO: Let it crash, there is no hash to replace in this case
                   mnesia:write(#files{name = Filename, hashes = [Hash]});
               [{_, _, Latest} | Rest] ->
                   NewLatest =
                       lists:map(fun(H) ->
                                    if H == HashToReplace ->
                                           Hash;
                                       true ->
                                           H
                                    end
                                 end,
                                 Latest),
                   Latestest =
                       case lists:member(Hash, NewLatest) of
                           true ->
                               NewLatest;
                           false ->
                               [Hash | NewLatest]
                       end,
                   mnesia:write(#files{name = Filename, hashes = [Latestest, Latest | Rest]})
           end,
           case mnesia:read(locations, Hash) of
               [] ->
                   case mnesia:read(cursor, Filename) of
                       [] ->
                           Location =
                               #locations{hash = Hash,
                                          size = erlang:byte_size(Content),
                                          offset = 0},
                           mnesia:write(Location);
                       [{_, _, Offset}] ->
                           Location =
                               #locations{hash = Hash,
                                          size = erlang:byte_size(Content),
                                          offset = Offset},
                           mnesia:write(Location)
                   end;
               [_] ->
                   already_registered
           end,
           case mnesia:read(cursor, Filename) of
               [] ->
                   mnesia:write(#cursor{filename = Filename, offset = erlang:byte_size(Content)});
               [Cursor] ->
                   mnesia:write(#cursor{filename = Filename,
                                        offset = Cursor#cursor.offset + erlang:byte_size(Content)})
           end
        end,
    {Hash, Function}.

compose([]) ->
    fun() -> nothing end;
compose(Commands) ->
    lists:foldr(fun({Hash, Function}, {HashAcc, FunAcc}) ->
                   {[Hash | HashAcc],
                    fun() ->
                       Function(),
                       FunAcc()
                    end}
                end,
                {[], fun() -> ok end},
                lists:map(fun(X) -> create_command(X) end, Commands)).

main() ->
    %% Files :: Filename => Hash list list
    %% FilesProc = spawn(?MODULE, keeper, [nothing]),
    %% Locations :: Hash => {size : int; offset: int}
    %% LocationsProc = spawn(?MODULE, keeper, [nothing]),
    {Hashes, Operations} = compose([{write, "blacklist", <<"catboys">>}]),
    Res = mnesia:transaction(Operations),
    {_, NewOperations} = compose([{write, "blacklist", <<"maidboys">>, hd(Hashes)}]),
    Res2 = mnesia:transaction(NewOperations),
    io:format("Stage 1: ~p\nStage 2: ~p\n", [Res, Res2]),
    Read =
        fun() ->
           case mnesia:read(files, "blacklist") of
               [] ->
                   [];
               [Content] ->
                   Content
           end
        end,
    Res3 = mnesia:transaction(Read),
    io:format("Stage 3: ~p\n", [Res3]),
    ok.

%% [[3],
%%  [3,4,1],
%%  [3,2,1]
%%  [2,1],
%%  [1],
%%  []]
