Solve sudoku puzzle using a backtracking algorithm written in erlang, it also
includes a concurrent variant and API to solve many puzzles simultaneously.
This `write-up <http://prataprc.github.io/sudoku-pathological.html`_ explains
the algorithm is better detail. The primary motive of writing this program is
to analyze performance of erlang-VM and its concurrent-paradigm. There is a
command line script to play with the puzzle solver. First clone this
repository and change to its root-directory.

To randomly generate a 3x3 sudoku puzzle and solve the same.

.. code-block:: bash

    $ bin/sudoku -c 3
    complexity:3 count:1 difficulty:60 seed:522708
    {3,
     {{8,0,0,1,0,0,0,6,0},
      {0,0,4,0,6,7,0,9,0},
      {5,6,0,0,9,0,3,0,1},
      {2,0,5,4,8,0,6,7,0},
      {0,0,6,3,7,0,8,2,5},
      {0,7,8,0,2,5,1,0,4},
      {4,2,1,0,5,3,0,8,6},
      {6,8,9,0,0,2,4,0,3},
      {0,5,0,8,4,0,9,1,0}}}
    Time taken to evaluate: 1584uS

`-c` switch specifies the complexity of the puzzle, available options are 2, 3
and 4. Note that a 3x3 puzzle will have 81 slots. `difficulty` is percentage
of slots that are pre-populated while generating the puzzle. In the above
example, 48 slots are pre-populated.

To generate the same puzzle once again pass the same seed value,

.. code-block:: bash

    $ bin/sudoku -c 3 -seed 522708
    complexity:3 count:1 difficulty:60 seed:522708
    {3,
     {{8,0,0,1,0,0,0,6,0},
      {0,0,4,0,6,7,0,9,0},
      {5,6,0,0,9,0,3,0,1},
      {2,0,5,4,8,0,6,7,0},
      {0,0,6,3,7,0,8,2,5},
      {0,7,8,0,2,5,1,0,4},
      {4,2,1,0,5,3,0,8,6},
      {6,8,9,0,0,2,4,0,3},
      {0,5,0,8,4,0,9,1,0}}}
    Time taken to evaluate: 1584uS

You can also configure the `difficulty` and `count` parameters using the `-d`
and `-n` switch.

.. code-block:: bash

    $ bin/sudoku -c 3 -n 2 -d 40 -seed 522708
    complexity:3 count:2 difficulty:40 seed:522708
    {3,
     {{8,0,0,1,0,0,0,6,0},
      {0,0,4,0,6,0,0,9,0},
      {0,0,0,0,9,0,3,0,1},
      {0,0,5,4,8,0,0,7,0},
      {0,0,6,3,7,0,0,2,5},
      {0,0,8,0,2,5,1,0,4},
      {0,0,0,0,5,3,0,8,6},
      {0,8,9,0,0,0,0,0,3},
      {0,5,0,0,0,0,0,1,0}}}
    {3,
     {{0,0,0,0,0,0,0,6,7},
      {0,0,4,5,0,0,0,9,0},
      {0,0,0,0,9,0,0,4,1},
      {2,0,0,4,0,0,6,7,0},
      {0,4,6,3,7,0,0,0,0},
      {0,0,8,0,0,5,0,0,0},
      {4,2,0,0,5,3,7,0,6},
      {6,0,0,0,1,2,4,0,3},
      {0,5,0,0,4,6,0,0,0}}}
    Time taken to evaluate: 19078uS

note that we have generated 2 puzzles with `-c` switch with 40% of slots
pre-populated. Since we have used the same seed value, the first puzzle is
same as in the previous run - except that only 32 slots are pre-populated.

You can also supply a pre-generated puzzle from a file. Make sure that the
puzzle is saved in erlang term-format.

.. code-block:: bash

    $ bin/sudoku -f priv/puzzle.term
    Time taken to evaluate: 12961uS

Now let us run some benchmark,

.. code-block:: bash

    $ bin/sudoku -c 3 -n 10 -d 40 -s 522708 -benchmark
    complexity:3 count:10 difficulty:40 seed:522708
    count   seq   parallel
    10    70175    34873
    9    56808    30271
    8    55088    31271
    7    41584    22020
    6    31972    17950
    5    29238    15961
    4    26638    15030
    3    24072    14265
    2    17886    9791
    1    9253    9306

above run generates and solves 10 3x3 puzzles with 40% of slot pre-populated
benchmarking the execution by solving them one after the other and then
simultaneously.

There is also a concurrent version of the algorithm. Let us repeat the
previous run in concurrent mode,

.. code-block:: bash

    $ bin/sudoku -c 3 -n 10 -d 40 -s 522708 -t -benchmark
    complexity:3 count:10 difficulty:40 seed:522708
    count   seq   parallel
    10    96202    77414
    9    96028    92655
    8    78767    63666
    7    71351    67198
    6    51281    38291
    5    49856    43204
    4    47960    35199
    3    35146    38394
    2    29881    36626
    1    21015    20211

`-t` switch enables the concurrent mode.

A more detailed analysis of erlang VM is available in this
`article <http://prataprc.github.io/sudoku-in-erlang.html>`_. For queries
please post to be directly via prataprc (at) gmail.com.

Have a nice time,

