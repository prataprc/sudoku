-module(sudoku_wst).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module APIs
-export([start_link/1, load/0, blur/0]).

-export([onload/1, onblur/1]).

% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("ncurses/include/ncurses.hrl").
-include("sudoku.hrl").

%---- module APIs

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

load() ->
    gen_server:call(?MODULE, load, infinity).

blur() ->
    gen_server:call(?MODULE, blur, infinity).

%---- gen_server callbacks

init(_Args) ->
    {ok, #wst{}}.


handle_call(load, _From, State) ->
    {reply, ok, ?MODULE:onload(State)};

handle_call(blur, _From, State) ->
    {reply, ok, ?MODULE:onblur(State)};

handle_call({S}, _From, State) ->
    {reply, S, State}.


handle_cast( _Request, State )->
    {noreply, State}.


handle_info( _Info, State )->
    {noreply, State}.


terminate( _Reason, State )->
    {ok, State}.


code_change( _OldVsn, State, _Extra )->
    {ok, State}.


%---- Handle events.

onload(State) ->
    {Yn, Xn} = ncdrv:getmaxyx(),
    {Y, X, Ys, Xs} = {Yn-1, 0, 1, Xn},
    Win = ncdrv:newwin(Ys, Xs, Y, X),
    State#wst{win=Win, y=Y, x=X, rows=Ys, cols=Xs}.

onblur(#wm{win=Win}=State) ->
    ncdrv:delwin(Win),
    State#wm{win=undefined}.


