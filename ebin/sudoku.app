{application, sudoku,
    [ {description,  "Sudoku puzzle generator and solver"},
      {vsn,          "0.1.0"},
      {modules,      [sudoku, sudoku_gen, sudoku_slv, sudoku_tbl, sudoku_v]},
      {registered,   [sudoku_sup]},
      {mod,          {sudoku, []}},
      {env,          [ {concurrent, true},
                       {procs, 100}
                     ]},
      {applications, [kernel, stdlib]}
    ]
}.
