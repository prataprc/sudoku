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
    Result = 
        try index() 
        catch
                throw:Term -> Term;
                exit:Reason -> {'EXIT',Reason};
                error:Reason -> {'EXIT',{Reason,erlang:get_stacktrace()}}
        end,
    %error_logger:info_msg("Result .... ~p~n", [Result]),
    Result.

index() ->
    XmlFile = filename:join([code:priv_dir(sudoku), "3play.xml"]),
    XPort = ncdrv:mainbox(),
    DocPath = ncpath:docpath(node(), sudoku, XmlFile),
    Doc = ncdom:xdoc(XPort, DocPath, XmlFile),
    ncdrv:loaddoc(Doc).


%---- module local functions.

get_childspec() ->
    {ok, {ReS, ChildSpec}} = application:get_env(childspec),
    {ReS, ChildSpec}.

