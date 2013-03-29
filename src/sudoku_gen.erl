%% Sudoku puzzle generator
%%
%% Gotcha : 
%%  1. The current implementation of puzzle generator is not a full proof way
%%     to generate a valid puzzle, since it is using a random algorithm in
%%     picking the numbers. The randomization algorithm is modularized into
%%     set of skip rules and pick rules can be extended to make the puzzle
%%     generator full proof.
%%
%% Notes :
%%  1. There is also a nice logic where by valid sudoku puzzles are generated
%%     by switching the numbers.
%%
%% TODO :
%%  1. Because the puzzle generator algorithm is not fool proof, unit-testing
%%     for pick_number(), gen_row(), gen_rows(), gen_table() functions are
%%     deferred !
%%  2. While picking the numbers (using pick rules), the algorithm does
%%     implement a 'difficulty' parameter for the generated puzzle.
%%  3. While hiding the numbers from a fully solved puzzle, the algorithm
%%     does not implement a 'difficulty' parameter for the generated puzzle.
%%  4. There seems to  be a dancing link algorithm popularized by Donald
%%     Knuth. Check out how valid they are.

% S    for Sudoku complexity
% D    for matrix Dimension
% R    Denotes Row
% C    Denotes Column

-module(sudoku_gen).
-author('prataprc@gmail.com').
-behaviour(gen_server).

-import(
    sudoku_tbl,
    [init_table/1, tblelement/3, update_table/4, nthcol/2, eintable/2,
     nthsubtab/3, subtab/4 ]).

% module APIs
-export([ start_link/1, generate/3, generate/4, switchnumbers/4,
          switchnumbers/2, hidenumbers/3 ]).

% behaviour callbacks
-export([
    handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2,
    code_change/3 ]).

-include("sudoku.hrl").

%---- module APIs.

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

% Generate a fresh sudoku puzzle. The puzzle is randomly generated, so can be
% a valid one or may not be.
generate( random, S, Elimit ) ->
    gen_server:call( ?MODULE, {genrandom, S, Elimit}, infinity );

% Repeatedly call generate(random,...) until a valid matrix is generated.
generate( valid, S, Elimit ) ->
    gen_server:call( ?MODULE, {genvalid, S, Elimit}, infinity ).

% Generate a sudoku puzzle based on a valid, solved puzzle, by switching the
% numbers.
generate( switching, Seed, S, Elimit ) ->
    gen_server:call( ?MODULE, {genswitching, Seed, S, Elimit}, infinity ).

switchnumbers( S, Table, Num1, Num2 ) ->
    switch( Num1, Num2, S, Table, 1, 1 ).

switchnumbers( S, Table ) ->
    N = S*S,
    Num  = random:uniform( N ),
    Num1 = random:uniform( N ),
    Num2 = if Num1 =:= Num -> ((Num1 + 1) rem N) + 1 ; true -> Num end,
    switch( Num1, Num2, S, Table, 1, 1 ).

hidenumbers( S, Table, Elimit ) ->
    hidenumbers( S, Table, Elimit, S*S*S*S ).

%---- gen_server callbacks

init( _Args ) ->
    { ok, [ {concurrent, application:get_env(concurrent)} ] }.

handle_call( {genrandom, S, Elimit}, _From, State ) ->
    { reply, genrandom(S, Elimit), State };

handle_call( {genvalid, S, Elimit}, _From, State ) ->
    { reply, genvalid(S, Elimit), State };

handle_call( {genswitching, Seed, S, Elimit}, _From, State ) ->
    { reply, genswitching(Seed, S, Elimit), State }.

handle_cast( Request, State )->
    io:format("cast ~w ~w~n", [Request, State]),
    {noreply, State}.

handle_info( _Info, State )->
    %%io:format("info ~w ~w~n", [Info, State]),
    {noreply, State}.

terminate( Reason, State )->
    io:format("terminate ~w ~w~n", [Reason, State]),
    {noreply, State}.

code_change( OldVsn, State, _Extra )->
    io:format("codechange ~w ~w~n", [OldVsn, State]),
    {noreply, State}.


%---- module local functions

% Randomly generate a number matrix and hope that it is a valid sudoku matrix.
genrandom( S, Elimit ) ->
    Sdk_init = #sudoku{ complexity=S, 
                        table=init_table( S ),
                        r=1,         %% Initial count
                        c=1,         %% Initial count
                        count=0,     %% Numbers in the table
                        rules=#rules{ elimit=Elimit }
                      },
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    (gen_table( Sdk_init, 5 ))#sudoku.table.

genvalid( S, Elimit ) ->
    Table = genrandom( S, Elimit ),
    case sudoku_slv:solve( S, Table ) of
        {passed, Tnew} -> Tnew;
        {failed, _} -> genvalid( S, Elimit )
    end.

genswitching(Seed, S, Elimit) ->
    random:seed(Seed, Seed, Seed),
    Table = switchnumbers(S, reftable(S)),
    hidenumbers(S, Table, Elimit).

% List of Skip-Rule functors. Skip rules are based on 'rules.elimit' and a
% randomized skip
sr_countlimit(#sudoku{count=C, rules=R}) when C < R#rules.elimit -> false;
sr_countlimit( _Sudoku ) -> true.

sr_random( #sudoku{complexity=S, rules=R} ) ->
    random:uniform(S*S*S*S) > R#rules.elimit.

skipfunctors() -> [ fun sr_countlimit/1, fun sr_random/1 ].


% Pick rules, don't pick if element 'E' is already found in the 
% row / column / subtable of table.
pr_row( E, #sudoku{table=Table, r=R} ) ->
    lists:all( fun(X) -> X =/= E end, tuple_to_list(element( R, Table )) ).

pr_col( E, #sudoku{table=Table, c=C} ) ->
    lists:all( fun(X) -> X =/= E end, nthcol( C, Table ) ).

pr_stb( E, #sudoku{complexity=S, table=Table, r=R, c=C} ) ->
    N = nthsubtab( R, C, S ),
    lists:all( fun(X) -> X =/= E end, subtab( asflis, S, N, Table )).

pickfunctors() ->  [ fun pr_row/2, fun pr_col/2, fun pr_stb/2 ].

% Generate puzzle by swapping elements.
switch( _, _, S, Table, R, _ ) when R > (S*S) -> Table;
switch( Num1, Num2, S, Table, R, C ) when C > (S*S) ->
    switch( Num1, Num2, S, Table, R+1, 1 );
switch( Num1, Num2, S, Table, R, C ) ->
    switch( Num1, Num2, S,
            case tblelement( Table, R, C ) of
                Num1 -> update_table( Table, R, C, Num2 );
                Num2 -> update_table( Table, R, C, Num1 );
                _    -> Table
            end,
            R, C+1 
          ).

% Hide numbers from a fully solved puzzle table.
hidenumbers( _, Table, Elimit, Elimit ) -> Table;
hidenumbers( S, Table, Elimit, Count ) ->
    R = random:uniform(S*S), 
    C = random:uniform(S*S), 
    E = tblelement(Table, R, C),
    case E of
        0 -> hidenumbers( S, Table, Elimit, Count );
        _ ->
            Tbnew = update_table(Table, R, C, 0),
            hidenumbers( S, Tbnew, Elimit, Count-1 )
    end.

% Randomly pick an element based on pick rules and skip rules. Just pray 
% that the randomly picked element collection is a valid sudoku puzzle.
pick_number( #sudoku{}=_Sudoku, [] ) -> %% Execution should never reach here
    0;
pick_number( #sudoku{}=Sudoku,  [H|T] ) ->
    case lists:all( fun(F) -> F(H, Sudoku) end, pickfunctors() ) of
        true  -> H;
        false -> pick_number( Sudoku, T )
    end.

pick_number( #sudoku{ complexity=S, table=Table, r=R, c=C }=Sudoku ) ->
    case tblelement( Table, R, C ) of   %% Fetch the table element
        0 -> 
            case lists:any( fun(F) -> F(Sudoku) end, skipfunctors() ) of
                true  -> {new, 0};
                false -> {new, pick_number( Sudoku, lists:seq( 1, S*S ))}
            end;
        N when N > 0 -> 
            { old, N }
    end.

% Randomly generate a row and eventually all rows, based on pick_number()
% logic.
gen_row( #sudoku{ complexity=S, c=C }=Sudoku ) when C > (S*S)->
    Sudoku;
gen_row( #sudoku{ table=Table, r=R, c=C, count=Count }=Sudoku ) ->
    case pick_number( Sudoku ) of
    { new, 0 } ->
        gen_row( Sudoku#sudoku{ table=update_table( Table, R, C, 0 ),
                                c=C+1 });
    { new, Element } ->
        gen_row( Sudoku#sudoku{ table=update_table( Table, R, C, Element ),
                                c=C+1, count=Count+1 });
    { old, Element } ->
        gen_row( Sudoku#sudoku{ table=update_table( Table, R, C, Element ),
                                c=C+1 })
    end.

gen_rows( #sudoku{ complexity=S, r=R }=Sudoku ) when R > (S*S) ->
    Sudoku;
gen_rows( #sudoku{ r=R }=Sudoku ) ->
    gen_rows( (gen_row( Sudoku#sudoku{ c=1 } ))#sudoku{ r=R+1 } ).

gen_table( Sudoku, 0 ) ->
    Sudoku;
gen_table( #sudoku{ complexity=S, rules=Rules }=Sudoku, Times ) ->
    Sdknew = gen_rows( Sudoku#sudoku{ r=1, c=1 } ),
    case ( eintable(S, Sdknew#sudoku.table) =:= Rules#rules.elimit ) of
        true  -> Sdknew;
        false -> gen_table( Sdknew, Times - 1 )
    end.

%---- valid sudoku table, used as reference for generating puzzles.

reftable(2) ->
    {{3,1,4,2},{4,2,1,3},{1,3,2,4},{2,4,3,1}};

reftable(3) ->
    {{8,7,2,1,3,4,5,6,9},
     {3,1,4,5,6,9,2,7,8},
     {5,6,9,2,7,8,3,4,1},
     {2,3,5,4,8,1,6,9,7},
     {1,4,6,3,9,7,8,2,5},
     {7,9,8,6,2,5,1,3,4},
     {4,2,1,7,5,3,9,8,6},
     {6,8,7,9,1,2,4,5,3},
     {9,5,3,8,4,6,7,1,2}};

reftable(4) ->
    {{4,5,6,7,1,8,2,9,3,10,11,12,13,14,15,16},
     {8,9,10,1,13,14,3,4,15,2,16,5,6,7,11,12},
     {3,11,12,13,5,6,15,16,1,7,8,14,4,2,9,10},
     {14,15,2,16,7,10,11,12,4,6,9,13,5,8,1,3},
     {5,6,7,8,2,9,10,1,11,12,13,15,16,3,4,14},
     {9,4,11,10,6,3,7,8,2,1,14,16,12,5,13,15},
     {12,13,1,3,14,16,4,15,5,8,6,7,2,9,10,11},
     {2,14,16,15,11,5,12,13,9,4,3,10,7,6,8,1},
     {6,7,5,2,3,4,8,10,12,9,15,11,1,16,14,13},
     {10,8,9,4,12,11,13,6,14,16,5,1,3,15,2,7},
     {11,16,3,14,9,15,1,5,6,13,7,2,8,10,12,4},
     {13,1,15,12,16,7,14,2,8,3,10,4,9,11,5,6},
     {7,2,8,9,4,12,16,11,10,14,1,3,15,13,6,5},
     {1,10,13,6,15,2,5,3,16,11,4,8,14,12,7,9},
     {15,3,14,11,8,1,9,7,13,5,12,6,10,4,16,2},
     {16,12,4,5,10,13,6,14,7,15,2,9,11,1,3,8}}.

