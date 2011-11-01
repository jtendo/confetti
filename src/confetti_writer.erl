%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Configuration saver.
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_writer).
-author('adam.rutkowski@jtendo.com').
-export([dump_config/3, store_working_config/4]).
-include("confetti.hrl").

%%%===================================================================
%%% API
%%%===================================================================

dump_config(Location = {_, _}, _, RawConf) ->
    dump(Location, RawConf).

store_working_config(ProviderName, Opts, Conf, RawConf) ->
    confetti_table_man:store({ProviderName, Opts, Conf, RawConf}).

%%%===================================================================
%%% Helpers
%%%===================================================================

dump(Location, OutConf) ->
    Fname = confetti_utils:fname(dump, Location),
    case file:write_file(Fname, OutConf) of
        ok -> {ok, Fname};
        {error, Reason} -> {error, Reason}
    end.


