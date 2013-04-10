-module(sudoku_curses).
-author('prataprc@gmail.com').
-export([start/0]).

-include_lib("sudoku/include/sudoku.hrl").
-include_lib("ncurses/include/ncurses.hrl").

start() ->
    application:start(ncurses),
    application:start(sudoku),

    ncdrv:app(sudoku),

    sudoku_wm:initialize(),
    sudoku_wst:initialize(),
    sudoku_wm:play(3),
    Reason = ncdrv:trapexit(),

    application:stop(sudoku),
    application:stop(ncurses),
    erlang:halt().
