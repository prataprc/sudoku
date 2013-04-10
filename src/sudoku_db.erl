-module(sudoku_db).
-author('prataprc@gmail.com').

% module APIs
-export([initdb/0, initdb/2, insert/2, lookup/2, delete/2]).

-include("sudoku.hrl").

initdb(DBFile, Opts) ->
    filelib:ensure_dir(DBFile),
    Name = dets:open_file(DBFile, Opts),
    {ok, Name}.

initdb() ->
    Home = os:getenv("HOME"),
    {ok, DBRoot} = application:get_env(dbroot),
    DBFile = filename:join(Home, DBRoot, "puzzles.dets"),
    Opts = [{access, read_write}, {auto_save, 180000}, {type, set}],
    initdb(DBFile, Opts).

insert(Name, {S, Matrix}=_Puzzle) ->
    Bucket = random:uniform(1, 10000),
    Entry = { {S, Bucket}, Matrix },
    dets:insert(Name, Entry).

lookup(Name, {_S, _Bucket}=Key) -> dets:lookup(Name, Key);
lookup(Name, S) ->
    Bucket = random:uniform(1, 10000),
    lookup(Name, {S, Bucket}).

delete(Name, {S, all}) -> dets:match_delete(Name, {S, '_'});
delete(Name, {all, _}) -> dets:delete_all_objects(Name);
delete(Name, {_S, _Bucket}=Key) -> dets:delete(Name, Key).
