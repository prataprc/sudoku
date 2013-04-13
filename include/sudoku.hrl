-define(Hn(S), ((S-1)*3 + 1)).
-define(ULCORNER, (?ACS_ULCORNER bor ?COLOR_PAIR(?COLOR_YB))).
-define(URCORNER, (?ACS_URCORNER bor ?COLOR_PAIR(?COLOR_YB))).
-define(LLCORNER, (?ACS_LLCORNER bor ?COLOR_PAIR(?COLOR_YB))).
-define(LRCORNER, (?ACS_LRCORNER bor ?COLOR_PAIR(?COLOR_YB))).
-define(LTEE,     (?ACS_LTEE bor ?COLOR_PAIR(?COLOR_YB))).
-define(RTEE,     (?ACS_RTEE bor ?COLOR_PAIR(?COLOR_YB))).
-define(TTEE,     (?ACS_TTEE bor ?COLOR_PAIR(?COLOR_YB))).
-define(BTEE,     (?ACS_BTEE bor ?COLOR_PAIR(?COLOR_YB))).
-define(HLINE,    (?ACS_HLINE bor ?COLOR_PAIR(?COLOR_YB))).
-define(VLINE,    (?ACS_VLINE bor ?COLOR_PAIR(?COLOR_YB))).
-define(PLUS,     (?ACS_PLUS bor ?COLOR_PAIR(?COLOR_YB))).

-define(DFTY_LIMIT(S, Difflty), erlang:trunc(S*S*S*S * (Difflty/100))).

% Describe how to generate a sudoku puzzle.
-record( puzzle, {
            complexity, % Size of sub-matrix, like 2,3,4.
            difficulty, % 1-100, number of valid elements in the puzzle.
            table,
            r,
            c,
            count
         }).

% Database to store valid sudoku matrix.
-record( sgen, {
            puzzledb
         }).

% Main window record can be used for ncurses frontend.
-record( wm, {
            view=play,
            win,
            y,
            x,
            rows,
            cols,
            s
         }).

% Status window record can be used for ncurses frontend.
-record( wst, {
            win,
            y,
            x,
            rows,
            cols
         }).
