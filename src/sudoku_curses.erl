-module(sudoku_curses).
-author('prataprc@gmail.com').
-export([start/0]).

-include_lib("sudoku/include/sudoku.hrl").
-include_lib("ncurses/include/ncurses.hrl").

start() ->
    application:start(ncurses),
    application:start(sudoku),
    %ncdrv:app(sudoku),
    %sudoku_wm:initialize(),
    %sudoku_wst:initialize(),
    %sudoku_wm:play(3),
    %Reason = ncdrv:trapexit(),
    %try sudoku_wm:initialize() of
    %    Reason -> io:format("~p~n", [Reason])
    %catch
    %    X:Y ->
    %        ncdrv:addstr( io_lib:format( "Exception type : ~p~n", [X] )),
    %        ncdrv:addstr( io_lib:format( "Exception : ~p~n", [Y] ));
    %    _:_ ->
    %        ncdrv:addstr( io_lib:format( "Unknown exception" ))
    %end,
    timer:sleep(100),
    application:stop(sudoku),
    application:stop(ncurses),
    %io:format("~p~n", [Reason]),
    erlang:halt().
