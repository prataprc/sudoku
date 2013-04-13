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
    application:start(ncurses),
    ?MODULE:start_link(Args).

prep_stop(State) ->
    State.

stop(State) ->
    State.

config_change(_Changed, _New, _Removed) -> ok.

%---- supervisor behaviour callbacks

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
    case lists:member(ncurses, init:get_plain_arguments()) of
        true ->
            XmlFile = filename:join([code:priv_dir(sudoku), "3play.xml"]),
            Node = ncdom:boxify(ncdrv:box(), ncdom:pagefile(XmlFile)),
            {ok, RootPid} = ncnode:start(none, Node),
            ncdrv:register_app({sudoku, XmlFile, RootPid});
        _ -> ok
    end.

init(_) ->
    {_, ChildSpec}=Specs = get_childspec(),
    case supervisor:check_childspecs(ChildSpec) of
        ok         -> {ok, Specs};
        {error, _} -> ignore
    end.

%---- module local functions.

get_childspec() ->
    {ok, {ReS, ChildSpec}} = application:get_env(childspec),
    {ReS, ChildSpec}.

