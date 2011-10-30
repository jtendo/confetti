-module(confetti_mgmt_cmnds).
-author('adam.rutkowski@jtendo.com').

-export([help/0]).
-export([cmds/0]).
-export([init/0, help/1]).
-export([reload/1, reload/2]).
-export([fetch/1]).
-export([cluster/0]).
-export([broadcast/1, broadcast/2]).

-define(HELP, [
        {"reload",
            {"Module",
                 "Reload local node Module configuration."}},

        {"fetch",
            {"Module",
                 "Fetch current Module config (in-memory working copy)"}},

        {"broadcast",
            {"Module [Nodes]",
                 "Broadcast local node working configuration copy to nodes Nodes."
                 "Defaults to cluster if Nodes not provided. See: 'help cluster'"}},

        {"cluster",
            {"",
                "Display node names available in cluster."
                "Current node is bypassed."}},

        {"help",
            {"Command",
                 "Display help on given command."
                 "Type 'cmds' for list of all available commands."}}
    ]).

%% just to make sure ?MODULE is loaded, FIXME code:load or something
init() ->
    ok.

cluster() ->
    string:join(lists:map(fun(N) -> atom_to_list(N) end, nodes()), ",").

fetch(Module) ->
    confetti_call(Module, fetch).

broadcast(Module) ->
    broadcast(Module, cluster()).

broadcast(_, []) ->
    "No neighbour nodes found. Verify your cluster settings.";

broadcast(Module, Nodes) ->
    io_lib:format("Broadcast ~p to ~p", [Module, Nodes]).

reload(Module) ->
    confetti_call(Module, reload).

reload(Module, _Nodes) ->
    confetti_call(Module, reload).

cmds() ->
    string:join(lists:sort(proplists:get_keys(?HELP)), "\n").

help() ->
    help("help").

help(Anything) ->
    case proplists:get_value(Anything, ?HELP) of
        undefined ->
            io_lib:format("Sorry, no help for ~p", [Anything]);
        {ExecHelp, HelpStr} ->
            io_lib:format("~s~n  usage:~n~n  > ~s ~s~n", [
                    HelpStr, Anything, ExecHelp
                ])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           internal functions                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

confetti_call(Module, Fun) ->
    try list_to_existing_atom(Module) of
        M ->
            io_lib:format("~p", [confetti:Fun(M)])
    catch _C:E ->
        throw({mgmt_call_failed, Fun, Module, E})
    end.

