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
    ?MODULE:start_link(Args).

prep_stop(State) ->
    State.

stop(State) ->
    State.

config_change(_Changed, _New, _Removed) -> ok.

%---- supervisor behaviour callbacks

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_) ->
    {_, ChildSpec}=Specs = get_childspec(),
    case supervisor:check_childspecs(ChildSpec) of
        ok         -> {ok, Specs};
        {error, _} -> ignore
    end.

%---- module local functions.

get_childspec() ->
    {ok, {ReS, ChildSpec}} = application:get_env(childspec),
    case lists:keyfind(ncurses, 1, application:which_applications()) of
        false ->
            {ReS, ChildSpec};
        _ ->
            {ok, WinSpec} = application:get_env(childspec_curses),
            {ReS, ChildSpec ++ WinSpec}
    end.

