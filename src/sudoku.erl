-module(sudoku).
-author('prataprc@gmail.com').
-behaviour(application).
-behaviour(supervisor).

% module APIs
-export([ ncstart/0 ]).

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

%---- module APIs.

ncstart() ->
    application:start(ncurses),
    application:start(sudoku),
    XmlFile = filename:join([code:priv_dir(sudoku), "3play.xml"]),
    {Y, X, Rows, Cols}=W = ncdrv:mainbox(),
    RootNode = ncdom:boxify(W, ncdom:pagefile(sudoku, XmlFile)),
    ncbuf:cursor_frames(Y, X, Rows, Cols, RootNode),
    ncnode:start({none, RootNode}),
    ncdrv:register_app({sudoku, RootNode}).


%---- module local functions.

get_childspec() ->
    {ok, {ReS, ChildSpec}} = application:get_env(childspec),
    {ReS, ChildSpec}.

