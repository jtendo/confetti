-module(confetti_mgmt_cmnds).
-author('adam.rutkowski@jtendo.com').

-export([init/0, help/1]).
-export([test/1, test/2, test/0]).

-help([
        {"test", {"[Arg|Arg1 Arg2]", "This command thanks you."}}
    ]).

%% just to make sure ?MODULE is loaded
init() ->
    ok.

test() ->
    "Whoa".

test(Arg) ->
    "Oh hai thanks!".

test(Arg1, Arg2) ->
    io_lib:format("Got ~p and ~p", [Arg1, Arg2]).

help(Anything) ->
    Help = proplists:get_value(help, ?MODULE:module_info(attributes)),
    case proplists:get_value(Anything, Help) of
        undefined ->
            io_lib:format("Sorry, no help for ~p", [Anything]);
        {ExecHelp, HelpStr} ->
            io_lib:format("~s ~s~n~s", [
                    Anything, ExecHelp, HelpStr
                ])
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


