-module(confetti_reader).
-author('adam.rutkowski@jtendo.com').
-export([load_config/1]).
-include("confetti.hrl").


%%%===================================================================
%%% API
%%%===================================================================

load_config(Opts = #confetti_opts{}) ->
    File = confetti_utils:fname(Opts),
    {ok, RawConfig} = file:read_file(File),
    case confetti_utils:u_consult(File) of
        {ok, Terms} ->
            case Opts#confetti_opts.callback_module of
                undefined -> {ok, RawConfig, Terms};
                Module ->
                    case Module:process_config_in(Terms) of
                    {ok, Config} ->
                        {ok, RawConfig, Config};
                    {error, Reason} ->
                        handle_error({File, Reason})
                end
            end;
        {error, Reason} -> handle_error({File, Reason})
    end.


%%%===================================================================
%%% Helpers
%%%===================================================================

handle_error(Error = {File, enoent}) ->
    io:format("Failed to load ~s - no such file.~n", [File]),
    Error;

handle_error(Error = {File, {StopLine, erl_parse, Reason}}) ->
    io:format("Failed to parse term in ~s close to line ~p: ~p~n", [File, StopLine, lists:flatten(Reason)]),
    Error;

handle_error(Error = {File, _}) ->
    io:format("Unknown error loading ~s: ~p~n", [File, Error]),
    Error.

