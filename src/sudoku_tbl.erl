-module(sudoku_tbl).
-author('prataprc@gmail.com').

-export([
    init_table/1, einrow/2, eintable/2, fixedelements/1, nthcol/2, 
    tblelement/3, update_table/4, flattable/2, nthsubtab/3, subtab/4,
    subtabrc/2, is_repeat/1, flush_buffer/0 ]).

-include("sudoku.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% S    for sudoku complexity
% D    for matrix Dimension

% Initialize row / table
init_table( S ) ->
    list_to_tuple( lists:duplicate( S*S, init_row(S*S) )).

init_row( Rlen ) ->     % module-local
    list_to_tuple( lists:duplicate( Rlen, 0 )).

% Count valid elements in the table
einrow([], Acc) -> Acc;
einrow([0|T], Acc) -> einrow(T, Acc);
einrow([_|T], Acc) -> einrow(T, Acc+1).

eintable(S, Table) ->
    Seq = lists:seq( 1, S*S ),
    lists:sum([ einrow( tuple_to_list( element( I, Table )), 0) || I <- Seq ]).

% In the given list, collect all the elements that are identified.
fixedelements( L ) ->
    [ X || X <- L, X =/= 0 andalso is_integer( X ) ].

% Get nth column from Table
nthcol( N, Table ) ->
    [ element( N, Row ) || Row <- tuple_to_list( Table ) ].

% Fetch Table element.
tblelement( Table, R, C ) ->
    element( C, element( R, Table )).

% Update Table element.
update_table( Table, R, C, E ) ->
    setelement( R, Table, setelement( C, element( R, Table ), E )).

% Flatten the entire Table 
flattable( S, Table ) ->
    lists:flatten([ tuple_to_list( element( R, Table ))
                    || R <- lists:seq( 1, S*S ) ]).

% Based on the row, column of an element, figure out the nth subtable it is
% located in.
nthsubtab( R, C, S ) ->
    (((R-1) div S) * S) + ((C-1) div S).    % Nth sub table, from 0.

% Get sub-table matrix.
subtab( aslist, S, N, Table )   ->          %% sub-table as list
    subtab( aslist, {S, N, Table} );
subtab( asflis, S, N, Table ) ->            %% sub-table as flatlist
    lists:flatmap( fun(X) -> X end, subtab( aslist, {S, N, Table} ));
subtab( astupl, S, N, Table )  ->           %% sub-table as tuple
    subtab( astupl, {S, N, Table} ).

subtabr( aslist, {From, To}, Row ) ->
    [ element(I, Row) || I <- lists:seq( From, To ) ];
subtabr( astupl, { From, To }, Row ) ->
    list_to_tuple(subtabr( aslist, { From, To }, Row )).
subtab( Aswhat, { S, N, Table } ) ->        %% N starts from 0
    From = ((N div S) * S) + 1,     %% From row
    To   = From+S-1,                %% To row
    Col  = ((N rem S) * S) + 1,
    [ subtabr( Aswhat, { Col, Col+S-1 }, element( X, Table ))
      || X <- lists:seq( From, To ) ].

%% Calculate the {R,C} tuple for each element in the subtable N
subtabrc( S, _, Count, Res ) when Count =:= (S*S) -> Res;
subtabrc( S, N, Count, Res ) ->
    R = (((N div S) *S) +1) + (Count div S),
    C = (((N rem S) *S) +1) + (Count rem S),
    subtabrc( S, N, Count+1, [{R, C} | Res] ).

subtabrc( S, N ) -> lists:reverse( subtabrc( S, N, 0, [] )).

%% Whether the adjusant elements are same.
is_repeat([]) -> false;
is_repeat([H | [H | _T]]) -> true;
is_repeat([_H|T]) -> is_repeat(T).

flush_buffer() ->
    receive
    _Any ->
        flush_buffer()
    after 0 ->
        true
    end.


-ifdef(TEST).

%---- eunit erlang unit test cases.

test_table() ->
    { {  0,  1,  0,  0 },
      {  2,  3,  0,  0 },
      {  0,  0,  0,  0 },
      {  1,  0,  0,  0 }
    }.

init_table_test_() ->
    [ ?_assertEqual( lists:sum( flattable( 3, init_table( 3 ))), 0 ) ].

ecount_test_() ->
    [
      ?_assertEqual( einrow([1,2,0,3], 0), 3 ),
      ?_assertEqual( eintable(2, test_table()), 4 )
    ].


nthcol_test_() ->
    [ ?_assertEqual( nthcol(1, test_table()), [0, 2, 0, 1 ]),
      ?_assertEqual( nthcol(2, test_table()), [1, 3, 0, 0 ]),
      ?_assertEqual( nthcol(3, test_table()), [0, 0, 0, 0 ]),
      ?_assertEqual( nthcol(4, test_table()), [0, 0, 0, 0 ])
    ].

tblelement_test_() ->
    [ ?_assertEqual( tblelement( test_table(), 1, 2 ), 1 ),
      ?_assertEqual( tblelement( test_table(), 2, 1 ), 2 ),
      ?_assertEqual( tblelement( test_table(), 2, 2 ), 3 ),
      ?_assertEqual( tblelement( test_table(), 4, 4 ), 0 )
    ].

update_table_test_() ->
    [ ?_assertEqual( tblelement(update_table(test_table(), 2, 2, [1,2]), 2, 2),
                     [1,2] )
    ].

flattable_test_() ->
    [ ?_assertEqual( flattable(2, test_table()),
                     [ 0, 1, 0, 0, 2, 3, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ] )
    ].

subtab_test_() -> 
    [ ?_assertEqual( subtab(asflis, 2, 0, test_table()), [0, 1, 2, 3] ),
      ?_assertEqual( subtab(asflis, 2, 1, test_table()), [0, 0, 0, 0] ),
      ?_assertEqual( subtab(asflis, 2, 2, test_table()), [0, 0, 1, 0] ),
      ?_assertEqual( subtab(asflis, 2, 3, test_table()), [0, 0, 0, 0] )
    ].

subtabrc_test_() -> 
    [ ?_assertEqual( subtabrc( 3, 0 ),
                     [ {X, Y} || X <- lists:seq(1,3), Y <- lists:seq(1,3) ] ),
      ?_assertEqual( subtabrc( 3, 1 ),
                     [ {X, Y} || X <- lists:seq(1,3), Y <- lists:seq(4,6) ] ),
      ?_assertEqual( subtabrc( 3, 2 ),
                     [ {X, Y} || X <- lists:seq(1,3), Y <- lists:seq(7,9) ] ),
      ?_assertEqual( subtabrc( 3, 3 ),
                     [ {X, Y} || X <- lists:seq(4,6), Y <- lists:seq(1,3) ] ),
      ?_assertEqual( subtabrc( 3, 4 ),
                     [ {X, Y} || X <- lists:seq(4,6), Y <- lists:seq(4,6) ] ),
      ?_assertEqual( subtabrc( 3, 5 ),
                     [ {X, Y} || X <- lists:seq(4,6), Y <- lists:seq(7,9) ] ),
      ?_assertEqual( subtabrc( 3, 6 ),
                     [ {X, Y} || X <- lists:seq(7,9), Y <- lists:seq(1,3) ] ),
      ?_assertEqual( subtabrc( 3, 7 ),
                     [ {X, Y} || X <- lists:seq(7,9), Y <- lists:seq(4,6) ] ),
      ?_assertEqual( subtabrc( 3, 8 ),
                     [ {X, Y} || X <- lists:seq(7,9), Y <- lists:seq(7,9) ] )
    ].

isrepeat_test_() ->
    [ ?_assertNot( is_repeat( [1,2,3,4,5] )),
      ?_assert( is_repeat( [1,1,3,4,5] )),
      ?_assert( is_repeat( [1,2,3,5,5] )),
      ?_assert( is_repeat( [1,2,3,0,0] )),
      ?_assert( is_repeat( [0,0,3,4,0] ))
    ].

-endif.
