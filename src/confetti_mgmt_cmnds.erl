-module(confetti_mgmt_cmnds).
-author('adam.rutkowski@jtendo.com').

-export([help/0]).
-export([cmds/0]).
-export([init/0, help/1]).
-export([reload/1, reload/2]).
-export([fetch/1]).

-help([
        {"reload",
            {"Module [CsvNodeList]",
             "Reload Module configuration, optionally on remote nodes."}},

        {"fetch",
            {"Module",
             "Fetch current Module config (in-memory)"}},

        {"help",
            {"Command",
             "Display help on given command. Type `cmds` for list of all "
             "available commands."}}
    ]).

%% just to make sure ?MODULE is loaded, FIXME code:load or something
init() ->
    ok.

fetch(Module) ->
    confetti_call(Module, fetch).

reload(Module) ->
    confetti_call(Module, reload).

reload(Module, _Nodes) ->
    confetti_call(Module, reload).

cmds() ->
    % FIXME relying on exports information is not the greatest idea
    %
    %Exports = ?MODULE:module_info(exports),
    %L = lists:map(fun({Cmd,Arity}) when Cmd =/= module_info ->
            %io_lib:format("~p/~p", [Cmd, Arity])
        %end, Exports),
    %string:join(L, "\n").
    "meh".

help() ->
    help("help").

help(Anything) ->
    Help = proplists:get_value(help, ?MODULE:module_info(attributes)),
    case proplists:get_value(Anything, Help) of
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


%format_arity(ArityInfo) ->
%    string:join(lists:map(fun integer_to_list/1, ArityInfo), " or ").

%discover_arity(F) ->
%    Exports = ?MODULE:module_info(exports),
%    try list_to_existing_atom(F) of
%        Func when is_atom(Func) ->
%            case proplists:get_all_values(Func, Exports) of
%                undefined -> "unknown number of";
%                ArityInfo -> format_arity(ArityInfo)
%            end;
%        _ -> "unknown number of"
%    catch _C:_E ->
%        "unknown number of"
%    end.


