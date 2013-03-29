-module(sudoku).
-author('prataprc@gmail.com').
-behaviour(application).
-behaviour(supervisor).

% application behaviour callbacks
-export([ start/2, prep_stop/1, stop/1, config_change/3 ]).

%% supervisor callbacks
-export([ init/1 ]).

% External exports
-export([ start_link/1 ]).

%---- application behaviour callbacks

% `Type` will either be normal | {takeover | Node} | {failover, Node},
% `Args` defined by the application specification key mod.
start(_Type, Args) ->
    case start_apps([]) of
        ok -> ?MODULE:start_link(Args);
        {error, Reason} -> {error, Reason}
    end.

prep_stop(State) ->
    State.

stop(State) ->
    State.

config_change(_Changed, _New, _Removed) -> ok.

%---- supervisor behaviour callbacks

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_) ->
    {ok, {{one_for_one, 10, 10},
          [{ sudoku_gen,
             {sudoku_gen, start_link, [[]]},
             permanent,
             5000,
             worker,
             [sudoku_gen, sudoku_tbl, sudoku_slv]},
           { sudoku_slv,
             {sudoku_slv, start_link, [[]]},
             permanent,
             5000,
             worker,
             [sudoku_slv, sudoku_tbl]}
          ]}}.

%---- module local functions.

start_apps([]) -> ok;
start_apps([App | T ]) ->
    case application:start(App) of
    ok ->
        start_apps(T);
    {error, {already_started, App}} ->
        start_apps(T);
    {error, _Reason} ->
        {error, {app_would_not_start, App}}
    end.


