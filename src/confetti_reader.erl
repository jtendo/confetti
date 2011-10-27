-module(confetti_reader).
-author('adam.rutkowski@jtendo.com').
-export([load_config/1, last_working_config/1]).
-include("confetti.hrl").


%%%===================================================================
%%% API
%%%===================================================================

load_config({ProviderName, Opts = {_, _}}) ->
    File = confetti_utils:fname(Opts),
    {ok, RawConfig} = file:read_file(File),
    case confetti_utils:u_consult(File) of
        {ok, Terms} ->
            confetti_utils:clear_alarm(ProviderName),
            confetti_writer:store_working_config(ProviderName, Opts, RawConfig, Terms),
            {ok, RawConfig, Terms};
        {error, Reason} ->
            confetti_utils:raise_alarm(ProviderName, Reason),
            handle_error({File, Reason})
    end.

last_working_config(ProviderName) ->
    case ets:lookup(confetti, ProviderName) of
        [] -> {error, no_previous_config};
        [{ProviderName, Opts, PrevConf, PrevRawConf}] ->
            {ok, {Opts, PrevConf, PrevRawConf}}
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================


handle_error(Error = {File, enoent}) ->
    io:format("Failed to load ~s - no such file.~n", [File]),
    {error, Error};

handle_error(Error = {File, {StopLine, erl_parse, Reason}}) ->
    io:format("Failed to parse term in ~s close to line ~p: ~p~n", [File, StopLine, lists:flatten(Reason)]),
    {error, Error};

handle_error(Error = {File, _}) ->
    io:format("Unknown error loading ~s: ~p~n", [File, Error]),
    {error, Error}.

