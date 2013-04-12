-module(sudoku_wm).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module APIs
-export([start_link/1, play/1, verify/1, event/2, load/0, blur/0]).
-export([onload/1, onblur/1]).

% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("ncurses/include/ncurses.hrl").
-include("sudoku.hrl").

%---- module APIs

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

play(Complexity) ->
    gen_server:call(?MODULE, {play, Complexity}, infinity).

verify(Complexity) ->
    gen_server:call(?MODULE, {verify, Complexity}, infinity).

load() ->
    gen_server:call(?MODULE, load, infinity).

blur() ->
    gen_server:call(?MODULE, blur, infinity).

event(inpch, Char) ->
    gen_server:cast(?MODULE, {inpch, Char}).

%---- gen_server callbacks

init(_Args) ->
    {ok, #wm{}}.


handle_call({play, S}, _From, State) ->
    NewState = State#wm{view=play, s=S},
    {reply, render(play, NewState), NewState};

handle_call({verify, S}, _From, State) ->
    {reply, S, State};

handle_call(load, _From, State) ->
    {reply, ok, ?MODULE:onload(State)};

handle_call(blur, _From, State) ->
    {reply, ok, ?MODULE:onblur(State)}.


handle_cast( {inpch, _Char}, State )->
    {noreply, State}.


handle_info( _Info, State )->
    {noreply, State}.


terminate( _Reason, State )->
    {ok, State}.


code_change( _OldVsn, State, _Extra )->
    {ok, State}.


%---- Handle events.

onload(State) ->
    ncdrv:app(sudoku),
    ncdrv:win(?WIN_MAIN),
    ncdrv:wdom(domtree()),
    {Yn, Xn} = ncdrv:getmaxyx(),
    {Y, X, Ys, Xs} = {0, 0, Yn-1, Xn},
    Win = ncdrv:newwin(Ys, Xs, Y, X),
    State#wm{win=Win}.

onblur(#wm{win=Win}=State) ->
    ncdrv:delwin(Win),
    ncdrv:wdom({empty, []}),
    ncdrv:win(undefined),
    ncdrv:app(undefined),
    State#wm{win=undefined}.


%-- Render sudoku puzzle in play mode.

render(play, #wm{cols=Xs, s=S})->
    {Y, X} = {4, 0},
    ncdrv:addstr(1, 2, string:centre("PLAY", Xs) ),
    Lines = lists:map( fun(L) -> string:centre(L, Xs) end, frame(do, S) ),
    NLines = lists:zip( lists:seq(0, length(Lines)-1), Lines ),
    lists:map( fun({N, L}) -> ncdrv:addchstr(Y+N, X, L) end, NLines ),
    ncdrv:refresh(),
    ok.
        
frame(do, S) ->
    First = frame(boxer, {S, ?ULCORNER, ?TTEE, ?URCORNER}),
    Middle = lists:reverse( frame( blocks, {S, []}) ),
    Last = frame(boxer, {S, ?LLCORNER, ?BTEE, ?LRCORNER}),
    [First] ++ Middle ++ [Last];

frame(blocks, {S, Acc}) -> 
    frame(blocks, {S, S, Acc});

frame(blocks, {_, 0, [_ | Acc]}) -> Acc;
frame(blocks, {S, N, Acc}) ->
    frame( blocks, {S, N-1, frame(block, {S, Acc})} );

frame(block, {S, Acc}) ->
    [ frame(boxer, {S, ?LTEE, ?PLUS, ?RTEE})
      | frame(rowpairs, {S, Acc}) ];

frame(rowpairs, {S, Acc}) ->
    SubRow1 = frame(subrow, {S, "0  ", $0}),
    SubRow2 = frame(subrow, {S, "   ", $ }),
    Line1 = lists:flatten( lists:duplicate( S, SubRow1 )) ++ [?VLINE],
    Line2 = lists:flatten( lists:duplicate( S, SubRow2 )) ++ [?VLINE],
    Rc = frame(rowpair, {S, S, Line1, Line2, Acc}),
    Rc;

frame(rowpair, {_, 1, Line1, _, Acc}) -> [ Line1 | Acc ];
frame(rowpair, {S, N, Line1, Line2, Acc}) ->
    frame( rowpair, {S, N-1, Line1, Line2, [Line2, Line1 | Acc]} );

frame(boxer, {S, X, Y, Z}) ->
    Rc = [X] ++ 
         lists:flatten( 
            lists:duplicate( S-1, lists:duplicate(?Hn(S), ?HLINE) ++ [Y] )
         ) ++
         lists:duplicate(?Hn(S), ?HLINE) ++
         [Z],
    Rc;

frame(subrow, {S, Str, Ch})->
    Rc = [?VLINE] ++ lists:flatten( lists:duplicate( S-1, Str )) ++ [Ch],
    Rc.


%---- local functions.

domtree() ->
    {?WIN_MAIN, [{?WIN_STATUS, []}]}.
