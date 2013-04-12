{application, sudoku,
    [ {description,  "Sudoku puzzle generator and solver"},
      {vsn,          "0.1.0"},
      {modules,      [sudoku, sudoku_gen, sudoku_slv, sudoku_tbl, sudoku_v,
                      sudoku_wm, sudoku_wst, sudoku_db]},
      {registered,   [sudoku, sudoku_gen, sudoku_slv, sudoku_wm, sudoku_wst]},
      {included_applications, [ncurses]},
      {mod,          {sudoku, []}},
      {env,          [ {concurrent, true},
                       {procs, 100},
                       {dbroot, '.textmode/sudoku'},
                       {childspec,
                            { {one_for_one, 10, 10},
                              [{sudoku_gen,
                                {sudoku_gen, start_link, [[]]},
                                permanent,
                                5000,
                                worker,
                                [sudoku_gen,sudoku_tbl,sudoku_db,sudoku_slv]},
                               {sudoku_slv,
                                {sudoku_slv, start_link, [[]]},
                                permanent,
                                5000,
                                worker,
                                [sudoku_v,sudoku_slv,sudoku_tbl]}
                              ]}},
                       {childspec_curses,
                            [{sudoku_wm,
                              {sudoku_wm, start_link, [[]]},
                              permanent,
                              5000,
                              worker,
                              [sudoku_wm]},
                             {sudoku_wst,
                              {sudoku_wst, start_link, [[]]},
                              permanent,
                              5000,
                              worker,
                              [sudoku_wst]}
                            ]}
                     ]},
                        
      {applications, [kernel, stdlib]}
    ]
}.

% vim: filetype=erlang:
