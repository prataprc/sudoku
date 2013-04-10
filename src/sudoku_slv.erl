%% Sudoku puzzle generator
%%
%% Notes :
%%  1. rr_fix() rule, reduce() and backtrack functions  are tested as part of
%%     the global test case.
%%  2. There is also a nice logic where by valid sudoku puzzles are generated
%%     by switching the numbers.
%%
%% TODO :
%%  1. Because the puzzle generator algorithm is not fool proof, unit-testing
%%     for pick_number(), gen_row(), gen_rows(), gen_table() functions are
%%     deferred !
%%  2. While picking the numbers (using pick rules), the algorithm does
%%     implement a 'difficulty' parameter for the generated puzzle.
%%  3. While hiding the numbbers from a fully solved puzzle, the algorithm
%%     does not implement a 'difficulty' parameter for the generated puzzle.
%%  4. There seems to  be a dancing link algorithm popularized by Donald
%%     Knuth. Check out how valid they are.

-module(sudoku_slv).
-author('prataprc@gmail.com').
-behaviour(gen_server).

-import(
    sudoku_tbl,
    [tblelement/3, update_table/4, fixedelements/1, nthcol/2, subtab/4,
     subtabrc/2, nthsubtab/3 ]).

% module APIs
-export([start_link/1, solve/2, psolve/1]).

% behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([rr_byrow/2, rr_bycol/2, rr_bystb/2, rr_uniqinrow/2, rr_uniqincol/2,
         rr_uniqinstb/2, entry/6, spawnrun/3 ]).

-include("sudoku.hrl").

%---- module APIs

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

solve(S, Table) ->
    gen_server:call( ?MODULE, {solve, S, Table}, infinity ).

psolve(Tables) ->
    gen_server:call( ?MODULE, {psolve, Tables}, infinity ).

%---- gen_server callbacks

init( _Args ) ->
    process_flag(trap_exit, true),
    ets:new(test, [set, named_table]),
    ets:insert(test, {busy, false}),
    {ok, {}}.

handle_call( {solve, S, Table}, _From, State ) ->
    ets:update_element(test, busy, {2, true}),
    Res = solve_( S, Table ),
    ets:update_element(test, busy, {2, false}),
    case Res of
        {passed, _} -> {reply, Res, State};
        {failed, _} -> {noreply, State}
    end;

handle_call( {psolve, Tables}, _From, State ) ->
    ets:update_element(test, busy, {2, true}),
    Results = psolve_( Tables, 0),
    ets:update_element(test, busy, {2, false}),
    Results = lists:filter( fun(Res) -> case Res of
                                        { passed, _ } -> true;
                                        { failed, _ } -> false
                                        end
                            end,
                            Results ),
    { reply, Results, State }.

handle_cast( Request, State )->
    io:format("cast ~w ~w~n", [Request, State]),
    {noreply, State}.

handle_info( _Info, State )->
    %%io:format("info ~w ~w~n", [Info, State]),
    {noreply, State}.

terminate( Reason, State )->
    io:format("terminate ~w ~w~n", [Reason, State]),
    ets:delete(test),
    {ok, State}.

code_change( OldVsn, State, _Extra )->
    io:format("codechange ~w ~w~n", [OldVsn, State]),
    {ok, State}.

%---- module local functions

% Initialize table's un-identified element with a list of all possible choices.
init_reduce( S, Table, R, _, _Allpos ) when R > (S*S) -> Table;
init_reduce( S, Table, R, C, Choice ) when C > (S*S) ->
    init_reduce( S, Table, R+1, 1, Choice );
init_reduce( S, Table, R, C, Choice ) ->
    case tblelement( Table, R, C ) of
        0 -> 
            Tb_new = update_table( Table, R, C, Choice ),
            init_reduce( S, Tb_new, R, C + 1, Choice );
        N when N =< (S*S) ->
            init_reduce( S, Table, R, C + 1, Choice )
    end.

init_reduce( S, Table ) ->
    init_reduce( S, Table, 1, 1, lists:seq( 1, S*S ) ).
    

% Rule to reduce the possible choice of elements, in relation to row. Choice
% cannot contain elements that are already identified in a row.
rr_byrow( S, Table, R, _, _Fixedrws ) when R > (S*S) -> Table;
rr_byrow( S, Table, R, C, Fixedrows ) when C > (S*S) ->
    rr_byrow( S, Table, R+1, 1, Fixedrows );
rr_byrow( S, Table, R, C, Fixedrows ) ->
    E = tblelement( Table, R, C ),
    if is_list( E ) ->
        Rowelem = element( R, Fixedrows ),
        rr_byrow(
            S, update_table(Table, R, C, E -- Rowelem), R, C+1, Fixedrows );
    true ->
        rr_byrow( S, Table, R, C+1, Fixedrows )
    end.

rr_byrow( S, Table ) ->
    Fixedrows = list_to_tuple([
                    fixedelements( tuple_to_list( element( R, Table )))
                    || R <- lists:seq( 1, S*S ) ]),
    rr_byrow( S, Table, 1, 1, Fixedrows ).


% Rule to reduce the possible list of elements, in relation to column. Choice
% cannot contain elements that are already identified in a column
rr_bycol( S, Table, R, _, _Fixedcls ) when R > (S*S) -> Table;
rr_bycol( S, Table, R, C, Fixedcols ) when C > (S*S) ->
    rr_bycol( S, Table, R+1, 1, Fixedcols );
rr_bycol( S, Table, R, C, Fixedcols ) ->
    E = tblelement( Table, R, C ),
    if is_list( E ) ->
        Colelem = element( C, Fixedcols ),
        rr_bycol(
            S, update_table( Table, R, C, E -- Colelem ), R, C+1, Fixedcols );
    true ->
        rr_bycol( S, Table, R, C+1, Fixedcols )
    end.

rr_bycol( S, Table ) ->
    Fixedcols = list_to_tuple([ fixedelements( nthcol( C, Table )) 
                                || C <- lists:seq( 1, S*S ) ]),
    rr_bycol( S, Table, 1, 1, Fixedcols ).


% Rule to reduce the possible list of elements, in relation to subtable. Choice
% cannot contain elements that are already identified in a sub-table.
rr_bystb( S, Table, R, _, _Fixdstbs ) when R > (S*S) -> Table;
rr_bystb( S, Table, R, C, Fixedstbs ) when C > (S*S) ->
    rr_bystb( S, Table, R+1, 1, Fixedstbs );
rr_bystb( S, Table, R, C, Fixedstbs ) ->
    E = tblelement( Table, R, C ),
    if is_list( E ) ->
        N = nthsubtab( R, C, S ),   % Nth sub table, from 0.
        Stbelem = element( N+1, Fixedstbs ),
        rr_bystb(
            S, update_table( Table, R, C, E -- Stbelem ), R, C+1, Fixedstbs );
    true ->
        rr_bystb( S, Table, R, C+1, Fixedstbs )
    end.

rr_bystb( S, Table ) ->
    Fixedstbs = list_to_tuple([ fixedelements( subtab( asflis,  S, I, Table ))
                                || I <- lists:seq( 0, (S*S)-1 ) ]),
    rr_bystb( S, Table, 1, 1, Fixedstbs ).


% When a possible combination is unique for a given slot, either row-wise or
% col-wise or subtable-wise, identify them.
find_unique( Choice, [] ) -> Choice;
find_unique( Choice, [H|T] ) when is_list(H) -> find_unique( Choice -- H, T );
find_unique( Choice, [_ | T] ) -> find_unique( Choice, T ).

find_unique( _, [], Res ) -> lists:reverse( Res );
find_unique( Upper, [H | T], Res ) ->
    if is_integer(H) ->
        find_unique( Upper, T, [[]|Res] );
    is_list( H ) ->
        find_unique( [H | Upper], T, [find_unique(H, Upper ++ T) | Res] )
    end.

find_unique( List ) ->
    find_unique( [], List, [] ).


% Helper function to update the Table based on the tuple {R,C,E} for each slot.
update_unique( Table, [] ) -> Table;
update_unique( Table, [{R,C,[E | []]} | T] ) ->
    update_unique( update_table( Table, R, C, [E] ), T );
update_unique( Table, [{_,_,_} | T] ) ->
    update_unique( Table, T ).

% Rule to reduce the table by identifying the unique possible element row-wise.
rr_uniqinrow( S, Table, R ) when R > (S*S) -> Table;
rr_uniqinrow( S, Table, R ) ->
    U = find_unique( tuple_to_list( element( R, Table ))),
    Z = lists:zip3( lists:duplicate( S*S, R ), lists:seq( 1, S*S ), U ),
    rr_uniqinrow( S, update_unique( Table, Z ), R+1 ).

rr_uniqinrow( S, Table ) -> rr_uniqinrow( S, Table, 1 ).
    

% Rule to reduce the table by identifying the unique possible element row-wise.
rr_uniqincol( S, Table, C ) when C > (S*S) -> Table;
rr_uniqincol( S, Table, C ) ->
    U = find_unique( nthcol( C, Table )),
    Z = lists:zip3( lists:seq( 1, S*S ), lists:duplicate( S*S, C ), U ),
    rr_uniqincol( S, update_unique(Table, Z), C+1 ).

rr_uniqincol( S, Table ) -> rr_uniqincol( S, Table, 1 ).
    

% Rule to reduce the table by identifying the unique possible element row-wise.
rr_uniqinstb( S, Table, I ) when I > ((S*S)-1) -> Table;
rr_uniqinstb( S, Table, I ) ->
    U = find_unique( subtab( asflis, S, I, Table )),
    Z = lists:zipwith( fun( {R, C}, E ) -> {R,C,E} end, subtabrc( S, I ), U),
    rr_uniqinstb( S, update_unique( Table, Z), I+1 ).

rr_uniqinstb( S, Table ) -> rr_uniqinstb( S, Table, 0 ).
    
% After all the reduction algorithms, if there is only one possible element
% in any given slot then fix the element in the slot.
rr_fix(S, Table, R, _, State) when R > (S*S) -> {State, Table};
rr_fix(S, Table, R, C, State) when C > (S*S) -> rr_fix(S, Table, R+1, 1, State);
rr_fix( S, Table, R, C, State ) ->
    case tblelement( Table, R, C ) of
        [E | []] ->     %% Yep. Fix it !!
            rr_fix( S, update_table( Table, R, C, E ), R, C+1, fixed );
        [_ | _] ->
            rr_fix( S, Table, R, C+1, State );
        E when is_integer( E ) ->
            rr_fix( S, Table, R, C+1, State );
        [] ->
            { notfixed, Table }
    end.

rr_fix( S, Table ) -> rr_fix( S, Table, 1, 1, notfixed ).


% List of Reduction-Rule functors 
reducerules() -> [ fun ?MODULE:rr_byrow/2, fun ?MODULE:rr_bycol/2,
                   fun ?MODULE:rr_bystb/2, fun ?MODULE:rr_uniqinrow/2,
                   fun ?MODULE:rr_uniqincol/2, fun ?MODULE:rr_uniqinstb/2
                 ].


% Repeated reduction until no more slots can be fixed using the functor rules.
reduce( S, Table, [] ) ->
    case rr_fix( S, Table ) of
        { notfixed, Tb_new } ->
            Tb_new;
        { fixed,    Tb_new } ->
            reduce( S, Tb_new, reducerules() ) %% Re-run
    end;
reduce( S, Table, [Fun|T] ) -> reduce( S, Fun( S, Table ), T ).


% The back-track algorithm to solve the sudoku puzzle. A double recursive
% algorithm that takes more effort to learn, take your time !!
backtrack( elist, _, Table, _, _, [] ) -> { failed, Table };
backtrack( elist, S, Table, R, C, [E|T] ) ->
    Tnew = update_table( Table, R, C, E ),
    case backtrack( reduce, S, Tnew, R, C+1 ) of
        { failed, _Tb_new } ->
            backtrack( elist, S, Table, R, C, T );
        { passed, Tb_new } ->
            { passed, Tb_new }
    end;
backtrack( elist, S, Table, R, C, E ) when is_integer(E) ->
    backtrack( elist, S, Table, R, C, [E] ).

% ------ Concurrent logic
is_concurrent() ->
    {ok, Procs} = application:get_env(procs),
    (application:get_env(concurrent) =:= {ok, true})
        andalso
        length(processes()) < Procs.

spanout( S, Table, R, C, [], Pids ) -> spanrecv( S, Table, R, C, Pids );
spanout( S, Table, R, C, [E | T], Pids ) ->
    case ets:lookup(test, busy) of
        [{busy, true}] ->
            Pid = spawn( ?MODULE, entry, [self(), S, Table, R, C, E] ),
            spanout( S, Table, R, C, T, [Pid | Pids] );
        [{busy, false}] ->
            {failed, Table}
    end;
spanout( S, Table, R, C, E, Pids ) when is_integer(E) ->
    spanout( S, Table, R, C, [E], Pids ).

spanrecv( _S, Table, _R, _C, [] ) -> {failed, Table};
spanrecv( S, Table, R, C, [Pid| Pids] ) ->
    erlang:process_flag(priority, high),
    receive
    {failed, _Tb_new} ->
        spanrecv( S, Table, R, C, Pids );
    {passed, Tb_new} ->
        killspans([ Pid | Pids ]),
        {passed, Tb_new}
    end.

killspans( [] ) -> ok;
killspans( [Pid | Pids] ) ->
    exit( Pid, kill ),
    killspans( Pids ).

entry( Frompid, S, Table, R, C, E ) ->
    erlang:process_flag(priority, low),
    Res = backtrack( elist, S, Table, R, C, [E | []] ),
    Frompid ! Res.

%% --------- Concurrent logic


backtrack( _, S, Table, R, _ ) when R > (S*S) -> { failed, Table };
backtrack( Atom, S, Table, R, C ) when C > (S*S) ->
    backtrack( Atom, S, Table, R+1, 1 );
backtrack( reduce, S, Table, R, C ) ->
    Tnew = reduce( S, Table, reducerules() ),
    case sudoku_v:fixtime( S, Tnew ) of
    true  ->    %% Vow, we achieved it, return back the SUDOKU TABLE !!
        { passed, Tnew };
    false ->    %% Check whether the partially formed table is proper
        case sudoku_v:genvalid( S, Tnew ) of
        true ->     %% Looks like, so proceed with backtracking
            case is_concurrent() of
                true ->     %% Concurrent backtracking
                    spanout( S, Tnew, R, C, tblelement( Tnew, R, C ), [] );
                false ->    %% Sequential backtracking
                    backtrack( elist, S, Tnew, R, C, tblelement( Tnew, R, C ))
            end;
        false ->
            { failed, Table }
        end
    end.

solve_( S, Table ) ->
    backtrack( reduce, S, init_reduce(S, Table), 1, 1 ).

psolve_(rcv, Count, Count, Results) -> Results;
psolve_(rcv, Count, N, Results) ->              %% Collect the results
    receive {solved, Res} -> psolve_( rcv, Count, N+1, [Res|Results] ) end.
psolve_([], N) ->
    psolve_(rcv, N, 0, [] );
psolve_( [{S, Table} | Tables], N ) ->
    %% Spawn a process for a puzzle
    spawn( ?MODULE, spawnrun, [self(), S, Table] ),
    psolve_(Tables, N+1).

spawnrun(Frompid, S, Table) ->
    Frompid ! {solved, solve_(S, Table)}.   %% Send back the result
