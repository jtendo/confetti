%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Configuration loader
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_reader).
-author('adam.rutkowski@jtendo.com').
-export([load_config/1, last_working_config/1]).
-include("confetti.hrl").

%%%===================================================================
%%% API
%%%===================================================================

load_config({ProviderName, Opts}) ->
    File = confetti_utils:fname(proplists:get_value(location, Opts)),
    case file:read_file(File) of
        {ok, RawConfig} ->
            case confetti_utils:u_consult(File) of
                {ok, Terms} ->


                    case proplists:get_value(validators, Opts, undefined) of
                        undefined ->
                            load_valid_config(ProviderName, Opts, RawConfig,
                                Terms);
                        Funs when is_list(Funs) ->
                            case validate_config(Funs, Terms) of
                                {ok, ValidTerms} ->
                                    load_valid_config(ProviderName, Opts,
                                        RawConfig, ValidTerms);
                                Err ->
                                    Err
                            end
                    end;

                {error, Reason} ->
                    confetti_utils:raise_alarm(ProviderName, Reason),
                    handle_error({File, Reason})
            end;
        {error, Err} ->
            handle_error({File, Err})
    end.

validate_config([], ValidTerms) ->
    {ok, ValidTerms};

validate_config([F|Rest], Terms) when is_function(F) ->
    try
        case apply(F, [Terms]) of
            {ok, ValidTerms} ->
                validate_config(Rest, ValidTerms);
            {error, Reason} ->
                {error, {invalid_config, Reason}}
        end
    catch _C:Error ->
        {error, {invalid_config, Error}}
    end.

load_valid_config(ProviderName, Opts, RawConfig, Terms) ->
    confetti_utils:clear_alarm(ProviderName),
    confetti_writer:store_working_config(ProviderName, Opts, RawConfig, Terms),
    {ok, RawConfig, Terms}.

last_working_config(ProviderName) ->
    case confetti_table_man:lookup(ProviderName) of
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

