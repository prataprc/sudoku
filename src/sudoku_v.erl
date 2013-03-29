-module(sudoku_v).
-author('prataprc@gmail.com').

-import(
    sudoku_tbl,
    [flattable/2, nthcol/2, fixedelements/1, subtab/4, is_repeat/1,
     fixedelements/1, eintable/2]).

-export([fixtime/2, genvalid/2, gentime/1 ]).

-include("sudoku.hrl").

% S  for sudoku complexity
% D  for matrix Dimension

% Validation logic for puzzle solver
fixtime( S, Table ) ->
    Seq = lists:seq( 1, S*S ),
    Seq1 = lists:seq( 0, S*S-1 ),
    Fn = fun(L) -> Seq =:= lists:sort(L) end,
    lists:all( fun erlang:is_integer/1, flattable(S, Table) )
    and
    lists:all( Fn, [ tuple_to_list(element( R, Table )) || R <- Seq ])
    and
    lists:all( Fn, [ nthcol(C, Table) || C <- Seq ])
    and
    lists:all( Fn, [ subtab(asflis, S, I, Table ) || I <- Seq1 ]).

% Validation logic for puzzle generator
genvalid( S, Table ) ->
    Seq = lists:seq(1, S*S),
    Fn = fun(L) -> 
            not is_repeat( lists:sort( fixedelements( L ))) end,
    lists:all( Fn, [ tuple_to_list( element( R, Table )) || R <- Seq ])
    and
    lists:all( Fn, [ nthcol(C, Table) || C <- Seq ])
    and
    lists:all( Fn, [ subtab(asflis, S, I-1, Table) || I <- Seq ]).


% Validation logic for puzzle generator
gentime( #sudoku{complexity=S, table=Table, rules=Rules} ) ->
    Seq = lists:seq(1, S*S),
    Seq1 = lists:seq( 0, S*S-1 ),
    Fn = fun(L) -> is_repeat( lists:sort( L )) end,
    not (
      ( eintable(S, Table) =/= Rules#rules.elimit )
      or
      lists:any( Fn, [ tuple_to_list( element( I, Table )) || I <- Seq ])
      or
      lists:any( Fn, [ nthcol(I, Table) || I <- Seq ])
      or
      lists:any( Fn, [ subtab(asflis, S, I, Table) || I <- Seq1 ])
    ).
