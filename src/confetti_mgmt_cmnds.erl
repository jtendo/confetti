%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Default management console commands bundle
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_mgmt_cmnds).
-author('adam.rutkowski@jtendo.com').

-export([reload/1, reload/2]).
-export([fetch/1]).
-export([cluster/0, cluster/1]).
-export([broadcast/1, broadcast/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 cluster                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cluster(help) ->
    "Display node names available in cluster. Current node is bypassed.\n"
    "Usage:\n\n"
    "> cluster".

cluster() ->
    case nodes() of
        [] ->
            "No neighbor nodes found.";
        Nodes ->
            string:join(lists:map(fun(N) -> atom_to_list(N) end, Nodes), ",")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  fetch                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch(help) ->
    "Fetch runtime module configuration\n"
    "Usage:\n\n"
    "> fetch mymodule";

fetch(Module) ->
    confetti_call(Module, fetch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                broadcast                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

broadcast(help) ->
     "Broadcast local node working configuration copy to nodes Nodes.\n"
     "Defaults to cluster if Nodes not provided. See: 'help cluster'.\n"
     "Usage:\n\n"
     "> broadcast node@host1,node@host2\n\n"
     "or\n"
     "> broadcast";

broadcast(Module) ->
    broadcast(Module, cluster()).

broadcast(_, []) ->
    "No neighbor nodes found. Verify your cluster settings.";

broadcast(Module, Nodes) ->
    % TODO implement me / rpc multicall
    io_lib:format("Broadcast ~p to ~p", [Module, Nodes]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 reload                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reload(help) ->
    "Reload local node Module configuration.\n"
    "Usage:\n\n"
    "> reload mymodule";

reload(Module) ->
    confetti_call(Module, reload).

reload(Module, _Nodes) ->
    confetti_call(Module, reload).





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

