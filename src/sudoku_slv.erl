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
         rr_uniqinstb/2, entry/4, solve_/2, psolve_/2, psolve_solve_/3 ]).

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
    {ok, []}.

handle_call( {solve, S, Table}, From, State ) ->
    Pid = spawn_link( ?MODULE, solve_, [S, Table] ),
    {noreply, [{Pid, From} | State]};

handle_call( {psolve, Tables}, From, State ) ->
    Pid = spawn_link( ?MODULE, psolve_, [Tables, 0] ),
    {noreply, [{Pid, From} | State]}.


handle_cast( Request, State )->
    error_logger:info_msg("sudoku_slv cast call : ~w ~w~n", [Request, State]),
    {noreply, State}.


handle_info( {'EXIT', Pid, {passed, _}=Res}, State )->
    From = proplists:get_value(Pid, State),
    gen_server:reply(From, Res),
    {noreply, proplists:delete(Pid, State)};

handle_info( {'EXIT', Pid, {invalid, _}=Res}, State )->
    From = proplists:get_value(Pid, State),
    gen_server:reply(From, Res),
    {noreply, proplists:delete(Pid, State)};

handle_info( {'EXIT', Pid, {psolve, Results}}, State )->
    From = proplists:get_value(Pid, State),
    gen_server:reply(From, Results),
    {noreply, proplists:delete(Pid, State)};

handle_info( {'EXIT', Pid, normal}, State )->
    {noreply, proplists:delete(Pid, State)}.

terminate( Reason, State )->
    error_logger:info_msg("sudoku_slv terminating : ~w~n", [Reason]),
    {noreply, State}.

code_change( OldVsn, State, _Extra )->
    error_logger:info_msg("sudoku_slv code-change : ~w ~w~n", [OldVsn, State]),
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
backtrack( elist, _, Table, _, _, [] ) -> {invalid, Table};
backtrack( elist, S, Table, R, C, [E|T]=Options ) ->
    case is_concurrent() of
        true ->
            spanout(S, Table, R, C, Options, 0); %% Concurrent backtracking
        false -> 
            Tnew = update_table( Table, R, C, E ),
            case backtrack(reduce, S, Tnew, R, C) of
                {invalid, _} -> backtrack(elist, S, Table, R, C, T);
                Res -> Res
            end
    end;
backtrack( elist, S, Table, R, C, E ) when is_integer(E) ->
    backtrack( elist, S, Table, R, C, [E] ).

backtrack( reduce, S, Table, R, C ) ->
    Tnew = reduce( S, Table, reducerules() ),
    case sudoku_v:fixtime( S, Tnew ) of
    true  ->    %% Vow, we achieved it, return back the SUDOKU TABLE !!
        {passed, Tnew};
    false ->    %% Check whether the partially formed table is proper
        case sudoku_v:genvalid( S, Tnew ) of
        true ->     %% Looks like, so proceed with backtracking
            movenext(S, Tnew, R, C+1);
        false ->
            {invalid, Table}
        end
    end.

% Exec may not match this clause ??
movenext(S, Table, R, _) when R > (S*S) -> {finfail, Table};
movenext(S, Table, R, C) when C > (S*S) -> movenext(S, Table, R+1, 1);
movenext(S, Table, R, C) ->
    Options = tblelement(Table, R, C),
    backtrack(elist, S, Table, R, C, Options).


% ------ Concurrent logic
is_concurrent() ->
    {ok, Procs} = application:get_env(procs),
    (application:get_env(concurrent) =:= {ok, true})
        andalso
        length(processes()) < Procs.

spanout( _, _, _, _, [], Count ) -> {concur, Count};
spanout( S, Table, R, C, [E | T], Count ) ->
    Tnew = update_table( Table, R, C, E ),
    spawn_link( ?MODULE, entry, [S, Tnew, R, C] ),
    spanout( S, Table, R, C, T, Count+1 ).

spanrcv( 0 ) -> {invalid, none};
spanrcv( Count ) ->
    receive
        {'EXIT', _, {passed, _}=Res} -> exit(Res);
        {'EXIT', _, _} -> spanrcv(Count -1)   % Fence all other failures.
    end.

entry(S, Table, R, C) ->
    process_flag(trap_exit, true),
    case backtrack(reduce, S, Table, R, C) of
        {concur, Count} -> exit(spanrcv(Count));
        Res -> exit(Res)
    end.

%% --------- Concurrent logic

solve_(S, Table) ->
    process_flag(trap_exit, true),
    case backtrack(reduce, S, init_reduce(S, Table), 1, 0) of
        {concur, Count} -> spanrcv(Count);
        Res -> exit(Res)
    end.


psolve_([], N) -> psolve_(N, 0, []);
psolve_( [{S, Table} | Tables], N ) ->
    spawn_link( ?MODULE, psolve_solve_, [self(), S, Table] ),
    psolve_(Tables, N+1).

psolve_solve_(Pid, S, Table) -> Pid ! ?MODULE:solve(S, Table).

psolve_(Count, Count, Acc) -> exit({psolve, Acc});
psolve_(Count, N, Acc) ->
    receive Res -> psolve_(Count, N+1, [Res|Acc]) end.

