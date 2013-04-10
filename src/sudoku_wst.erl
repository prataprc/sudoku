-module(sudoku_wst).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module APIs
-export([start_link/1, initialize/0]).

% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("ncurses/include/ncurses.hrl").
-include("sudoku.hrl").

%---- module APIs

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

initialize() ->
    gen_server:call(?MODULE, initialize, infinity).

%---- gen_server callbacks

init(_Args) ->
    {ok, #wst{}}.

handle_call(initialize, _From, State) ->
    {Yn, Xn} = ncdrv:getmaxyx(),
    {Y, X, Ys, Xs} = {Yn-1, 0, 1, Xn},
    Win = ncdrv:newwin(Ys, Xs, Y, X),
    NewState = State#wst{win=Win, y=Y, x=X, rows=Ys, cols=Xs},
    {reply, ok, NewState};

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


