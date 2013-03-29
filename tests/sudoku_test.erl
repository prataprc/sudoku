-module(sudoku_test).
-author('prataprc@gmail.com').

-behaviour(gen_server).

% Behaviour APIs.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3 ]).

% Server APIs.
-export([ start_link/1, run/0, run/3, run/4 ]).

-record(testcase, { complexity, elimit, count }).

-include_lib("eunit/include/eunit.hrl").
-include("sudoku.hrl").

pathologicalseq() ->
    {{0,0,6,7,0,0,2,0,0,0,0,0,0,0,15,16},
     {0,0,0,1,0,0,3,0,0,2,16,0,0,7,8,0},
     {3,0,0,13,5,0,0,16,1,7,0,0,4,2,0,0},
     {14,15,0,16,0,0,8,12,0,0,0,0,0,0,0,3},
     {0,0,7,11,0,0,10,1,8,0,0,0,0,0,0,14},
     {0,4,8,0,6,3,0,11,2,0,0,0,0,5,13,0},
     {12,0,0,0,14,0,4,15,5,11,0,0,0,9,10,8},
     {0,14,16,0,0,5,12,13,0,0,0,10,7,6,11,0},
     {6,0,5,0,0,0,11,10,12,9,0,8,0,16,0,0},
     {0,11,0,0,0,0,13,6,14,0,0,0,0,0,0,0},
     {8,16,3,0,0,0,1,0,0,13,7,0,0,10,0,0},
     {0,1,15,12,16,7,0,0,11,0,10,0,9,8,5,0},
     {7,0,0,0,0,0,16,0,0,14,0,0,0,0,6,5},
     {0,10,0,6,0,2,0,3,16,8,0,0,14,0,7,9},
     {15,3,0,0,0,0,9,0,0,5,12,6,10,0,0,2},
     {0,0,0,5,10,0,0,0,7,0,2,0,8,0,0,0}
    }.

%% ----------------- Exported functions -----------------

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

run() ->
    gen_server:call( ?MODULE, runonce, infinity ).

run(Size, Count, Elimit) ->
    gen_server:call( ?MODULE, {run, Size, Count, Elimit}, infinity ).

run(seq, Size, Count, Elimit) ->
    gen_server:call( ?MODULE, {seqrun, Size, Count, Elimit}, infinity );

run(parallel, Size, Count, Elimit) ->
    gen_server:call( ?MODULE, {parrun, Size, Count, Elimit}, infinity ).


%% ---------------- gen_server callbacks -------------------

init( Args ) ->
    { ok, Args }.

handle_call( runonce, _From, State ) ->
    { reply, runonce(), State };

handle_call( {run, Size, Count, Elimit}, _From, State ) ->
    { reply, run(Size, Count, Elimit, 0, []), State };

handle_call( {seqrun, Size, Count, Elimit}, _From, State ) ->
    { reply, seqrun(Size, Count, Elimit, 0, []), State };

handle_call( {parrun, Size, Count, Elimit}, _From, State ) ->
    { reply, parrun(Size, Count, Elimit), State }.

handle_cast( Request, State )->
    io:format("cast ~w ~w~n", [Request, State]),
    {noreply, State}.

handle_info( _Info, State )->
    %%io:format("info ~w ~w~n", [Info, State]),
    {noreply, State}.

terminate( Reason, State )->
    io:format("terminat ~w ~w~n", [Reason, State]),
    {ok, State}.

code_change( OldVsn, State, _Extra )->
    io:format("codechange ~w ~w~n", [OldVsn, State]),
    {ok, State}.


%% ---------------- Internal logic ------------------------

runonce() ->
    sudoku_slv:solve(4, pathologicalseq()).

run(_Size, Count, _Elimit, Count, Time) ->
    io:format("Solved ~w puzzles in ~w uS ~n", [ Count, lists:reverse(Time) ]);
run(Size, Count, Elimit, N, Time) ->
    Table = sudoku_gen:generate(switching, N+1, Size, Elimit),
    St    = now(),
    {passed, _} = sudoku_slv:solve(Size, Table),
    Dur   = timer:now_diff(now(), St),
    timer:sleep(10000),
    run(Size, Count, Elimit, N+1, [Dur | Time] ).

seqrun(_Size, Count, _Elimit, Count, Time) ->
    io:format("Solved ~w puzzles in ~w uS ~n", [ Count, lists:reverse(Time) ]);
seqrun(Size, Count, Elimit, N, Time) ->
    Table = sudoku_gen:generate(switching, N+1, Size, Elimit),
    St    = now(),
    {passed, _} = sudoku_slv:solve(Size, Table),
    Dur   = timer:now_diff(now(), St),
    seqrun(Size, Count, Elimit, N+1, [Dur | Time] ).

parrun(Size, Count, Elimit) ->
    Tables = [ {Size, sudoku_gen:generate(switching, N, Size, Elimit)} 
               || N <- lists:seq(1,Count) ],
    St    = now(),
    _     = sudoku_slv:psolve(Tables),
    Dur   = timer:now_diff(now(), St),
    io:format("Solved ~w puzzles in ~w uS ~n", [ Count, Dur ]),
    Dur.

%---- unit test cases for sudoku_tbl

print_table( D, Table ) ->
    [ io:format("~w~n", [element(I,Table)] ) || I <- lists:seq(1,D*D) ],
    io:format("~n").

test_table() ->
    { {  0,  1,  0,  0 },
      {  2,  3,  0,  0 },
      {  0,  0,  0,  0 },
      {  1,  0,  0,  0 }
    }.

sample_repeat() ->
    [ [ 1,2,3,4,5],
      [ 1,1,2,3,3],
      [ 1,2,3,4,4]
    ].


%---- unit test cases for sudoku_gen for sudoku_slv

swn_generator( Table ) ->
    [ ?_assert(
        sudoku_v:fixtime( 3, sudoku_gen:switchnumbers( 3, Table, 1, 4 ))),
      ?_assert(
        sudoku_v:fixtime( 3, sudoku_gen:switchnumbers( 3, Table )))
    ].
swn_test_() ->   %% switchnumbers
    {passed, Table} = sudoku_slv:solve(3, sudoku_gen:generate( valid, 3, 31 )),
    {setup, fun() -> Table end, fun swn_generator/1}.


hdn_validate( Table ) ->
    case sudoku_slv:solve( 3, Table ) of
        { passed, _ } -> true;
        { failed, _ } -> false
    end.
hdn_generator( Table ) ->
    [ ?_assert( hdn_validate( sudoku_gen:hidenumbers( 3, Table, 50 ))),
      ?_assert( hdn_validate( sudoku_gen:hidenumbers( 3, Table, 40 ))),
      ?_assert( hdn_validate( sudoku_gen:hidenumbers( 3, Table, 35 ))),
      ?_assert( hdn_validate( sudoku_gen:hidenumbers( 3, Table, 30 ))),
      ?_assert( hdn_validate( sudoku_gen:hidenumbers( 3, Table, 20 )))
    ].
hdn_test_() ->
    {passed, Table} = sudoku_slv:solve( 3, sudoku_gen:generate( valid, 3, 31 )),
    {setup, fun() -> Table end, fun hdn_generator/1}.


sr_countlimit_test_() ->
    [ ?_assert( not begin Rules = #rules{ elimit=10 },
                          Tb = #sudoku{ count=9, rules=Rules },
                          sudoku_gen:sr_countlimit( Tb ) end
              ),
      ?_assert( begin Rules = #rules{ elimit=10 },
                      Tb = #sudoku{ count=10, rules=Rules },
                      sudoku_gen:sr_countlimit( Tb ) end
              )
    ].

pr_row_test_() ->
    [ ?_assert(
            begin
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                Tb = #sudoku{ complexity=2, table=Table, r=2, c=1,
                             count=0, rules=#rules{ elimit=5 }
                           },
                sudoku_gen:pr_row( 1, Tb )
            end
      ),
      ?_assert(
            not
            begin
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                Tb = #sudoku{ complexity=2, table=Table, r=2, c=1,
                             count=0, rules=#rules{ elimit=5 }
                           },
                sudoku_gen:pr_row( 2, Tb )
            end
      )
    ].

pr_col_test_() ->
    [ ?_assert(
            begin
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                Tb = #sudoku{ complexity=2, table=Table, r=2, c=2,
                             count=0, rules=#rules{ elimit=5 }
                           },
                sudoku_gen:pr_col( 4, Tb )
            end
      ),
      ?_assert(
            not
            begin
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                Tb = #sudoku{ complexity=2, table=Table, r=2, c=2,
                             count=0, rules=#rules{ elimit=5 }
                           },
                sudoku_gen:pr_col( 1, Tb )
            end
      )
    ].

pr_stb_test_() ->
    [ ?_assert(
            begin
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                Tb = #sudoku{ complexity=2, table=Table, r=1, c=1,
                             count=0, rules=#rules{ elimit=5 }
                           },
                sudoku_gen:pr_stb( 4, Tb )
            end
      ),
      ?_assert(
            not
            begin
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                Tb = #sudoku{ complexity=2, table=Table, r=2, c=2,
                             count=0, rules=#rules{ elimit=5 }
                           },
                sudoku_gen:pr_stb( 1, Tb )
            end
      )
    ].

fixedelements_test_() ->
    [ ?_assert(
        sudoku_tbl:fixedelements( tuple_to_list( element( 2, test_table() )))
        =:=
        [ 2, 3 ]
      ),
      ?_assert(
        sudoku_tbl:fixedelements( tuple_to_list( element( 3, test_table() )))
        =:=
        []
      ),
      ?_assert( 
            begin 
                Table = sudoku_slv:init_reduce( 2, test_table() ),
                sudoku_tbl:fixedelements( tuple_to_list( element( 3, Table )))
                =:=
                []
            end
      )
    ].

init_reduce_generator( Table ) ->
    [ ?_assert( sudoku_tbl:tblelement( Table, 1, 1 ) =:= [ 1, 2, 3, 4 ] ),
      ?_assert( sudoku_tbl:tblelement( Table, 2, 2 ) =:= 3 )
    ].
init_reduce_test_() ->
    { setup, fun() -> sudoku_slv:init_reduce( 2, test_table() ) end,
             fun init_reduce_generator/1 }.


rr_byrow_generator( Table ) ->
    [ ?_assert( sudoku_tbl:tblelement( Table, 1, 1 ) =:= [ 2, 3, 4 ] ),
      ?_assert( sudoku_tbl:tblelement( Table, 1, 2 ) =:= 1 ),
      ?_assert( sudoku_tbl:tblelement( Table, 2, 2 ) =:= 3 ),
      ?_assert( sudoku_tbl:tblelement( Table, 2, 4 ) =:= [ 1, 4 ] )
    ].
rr_byrow_test_() ->
    { setup,
      fun() ->
        sudoku_tbl:rr_byrow( 2, sudoku_slv:init_reduce( 2, test_table() )) end,
      fun rr_byrow_generator/1 }.

rr_bycol_generator( Table ) ->
    [ ?_assertEqual( sudoku_tbl:tblelement( Table, 1, 1 ), [ 3, 4 ] ),
      ?_assertEqual( sudoku_tbl:tblelement( Table, 2, 4 ), [ 1, 2, 3, 4 ] ),
      ?_assertEqual( sudoku_tbl:tblelement( Table, 2, 1 ), 2 )
    ].
rr_bycol_test_() ->
    { setup,
      fun() -> 
        sudoku_slv:rr_bycol( 2, sudoku_slv:init_reduce( 2, test_table() )) end,
      fun rr_bycol_generator/1 }.

rr_bystb_generator( Table ) ->
    [ ?_assert( sudoku_tbl:tblelement( Table, 1, 1 ) =:= [ 4 ] ),
      ?_assert( sudoku_tbl:tblelement( Table, 1, 2 ) =:= 1 ),
      ?_assert( sudoku_tbl:tblelement( Table, 4, 4 ) =:= [ 1, 2, 3, 4 ] )
    ].
rr_bystb_test_() ->
    { setup,
      fun() ->
        sudoku_slv:rr_bystb( 2, sudoku_slv:init_reduce( 2, test_table() )) end,
      fun rr_bystb_generator/1 }.

rr_urow_generator( Table ) ->
    [ ?_assertEqual( sudoku_tbl:tblelement( Table, 4, 2 ), [2] ),
      ?_assertEqual( sudoku_tbl:tblelement( Table, 4, 3 ), [3,4] ),
      ?_assertEqual( sudoku_tbl:tblelement( Table, 4, 4 ), [3,4] )
    ].
rr_urow_test_() ->
    T1 = sudoku_tbl:update_table( 
            sudoku_tbl:update_table( test_table(), 1, 3, 2 ),
            2, 4, 2
         ),
    T2 = sudoku_slv:rr_bystb( 1,
            sudoku_slv:rr_bycol( 2,
                sudoku_slv:rr_byrow( 2, sudoku_slv:init_reduce( 2, T1 )
         ))),
    { setup,
      fun() -> sudoku_slv:rr_uniqinrow( 2, T2 ) end, fun rr_urow_generator/1
    }.

rr_ucol_generator( Table ) ->
    [ ?_assertEqual( sudoku_tbl:tblelement( Table, 2, 4 ), [1] ) ].
rr_ucol_test_() ->
    T1 = sudoku_tbl:update_table( test_table(), 3, 3, 1 ),
    T2 = sudoku_slv:rr_bystb( 1,
            sudoku_slv:rr_bycol( 2,
                sudoku_slv:rr_byrow( 2, sudoku_slv:init_reduce( 2, T1 )
         ))),
    { setup,
      fun() -> sudoku_slv:rr_uniqincol( 2, T2 ) end, fun rr_ucol_generator/1
    }.

rr_ustb_generator( Table ) ->
    [ ?_assertEqual( sudoku_tbl:tblelement( Table, 1, 3), [2] ) ].
rr_ustb_test_() ->
    T1 = sudoku_tbl:update_table( test_table(), 4, 4, 2 ),
    T2 = sudoku_slv:rr_bystb( 1,
            sudoku_slv:rr_bycol( 2,
                sudoku_slv:rr_byrow( 2, sudoku_slv:init_reduce( 2, T1 )
         ))),
    { setup,
      fun() -> sudoku_slv:rr_uniqinstb( 2, T2 ) end,
      fun rr_ustb_generator/1 }.


tcases_2D() ->
    [ { 2, 8,  10 }, { 2, 7,  10 }, { 2, 6,  10 }, { 2, 4,  10 } ].
tcases_3D() ->
    [ { 3, 35, 10 }, { 3, 31, 10 }, { 3, 27, 10 }, { 3, 20, 10 } ].
tcases_4D() ->
    [ { 4, 110, 10 }, { 4, 105, 10 }, { 4, 100, 10 }, { 4, 85, 10 } ].

tcases_234D() -> tcases_2D() ++ tcases_3D() ++ tcases_4D().

testfull( _ValidT, { D, Elimit, 0 }, PCount, FCount ) -> 
    io:format("Completed ~w dimension sudoku with Elimit, ~w ~n", [D,Elimit]),
    io:format("Passed ... ~w ~n", [PCount] ),
    io:format("Failed ... ~w ~n", [FCount] ),
    { PCount, FCount };

testfull( ValidT, { D, Elimit, Count }, PCount, FCount ) ->
    Table = sudoku_gen:hidenumbers(
                    D, sudoku_gen:switchnumbers( D, ValidT ), Elimit ),
    case sudoku_slv:solve( D, Table ) of
        { passed, _Tb_new } ->
            testfull( ValidT, { D, Elimit, Count-1 }, PCount+1, FCount );
        { failed, _Tb_new } ->
            testfull( ValidT, { D, Elimit, Count-1 }, PCount, FCount+1 )
    end.

testfull( d2 ) ->
    { passed, ValidT } = sudoku_slv:solve( 
                                2, sudoku_gen:generate( valid, 2, 10 )),
    [ testfull( ValidT, Tcase, 0, 0 ) || Tcase <- tcases_2D() ];
testfull( d3 ) ->
    { passed, ValidT } = sudoku_slv:solve(
                                3, sudoku_gen:generate( valid, 3, 40 )),
    [ testfull( ValidT, Tcase, 0, 0 ) || Tcase <- tcases_3D() ];
testfull( d4 ) ->
    { passed, ValidT } = sudoku_slv:solve(
                                4, sudoku_gen:generate( valid, 4, 50 )),
    [ testfull( ValidT, Tcase, 0, 0 ) || Tcase <- tcases_4D() ].

%% Manual test cases.
tcases_generate() ->
    [ #testcase{ complexity=3, elimit=27, count=10 },
      #testcase{ complexity=3, elimit=29, count=10 },
      #testcase{ complexity=3, elimit=31, count=10 },
      #testcase{ complexity=3, elimit=33, count=10 },
      #testcase{ complexity=3, elimit=35, count=10 },
      #testcase{ complexity=3, elimit=37, count=10 },
      #testcase{ complexity=4, elimit=100, count=10 },
      #testcase{ complexity=4, elimit=105, count=10 },
      #testcase{ complexity=4, elimit=110, count=10 },
      #testcase{ complexity=4, elimit=120, count=10 },
      #testcase{ complexity=4, elimit=130, count=10 }
    ].


test( generate, #testcase{ complexity=D, elimit=L }=Tc, Count ) ->
    Tb = sudoku_gen:generate( D, L ),
    sudoku_v:gentime(Tb),
    case sudoku_v:gentime(Tb) of
        true  -> true;
        false -> io:format("Failed Test case: ~w (Count ~w) ~nTable : ~w ~n",
                           [ Tc, Count, Tb ]), false
    end.
test( gencases ) ->
    lists:all( 
        fun( X ) -> X end,
        [ test( generate, Tc, Count ) ||
          #testcase{}=Tc <- tcases_generate(),
          Count <- lists:seq( 1, Tc#testcase.count ) ]
    );
test( backtrack ) ->
    sudoku_slv:backtrack( 
        reduce, 2, sudoku_slv:init_reduce( 2, test_table() ), 1, 1 );
test( solve ) ->
    Tbs = [ sudoku_gen:generate( 3, 20 ) || _I <- lists:seq(1,1)  ],
    print_table( 3, (lists:nth( 1, Tbs ))#sudoku.table),
    [ sudoku_slv:solve( 3, Table ) || #sudoku{table=Table}=_Tb <- Tbs ].

