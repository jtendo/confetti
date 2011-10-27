-module(confetti_writer).
-author('adam.rutkowski@jtendo.com').
-export([dump_config/3, store_working_config/4]).
-include("confetti.hrl").

%%%===================================================================
%%% API
%%%===================================================================

dump_config(Opts, _, RawConf) ->
    dump(Opts, RawConf).

store_working_config(ProviderName, Opts, Conf, RawConf) ->
    ets:insert(confetti, {
            ProviderName, Opts, Conf, RawConf
        }).

%%%===================================================================
%%% Helpers
%%%===================================================================

dump(Opts, OutConf) ->
    Fname = confetti_utils:fname(dump, Opts),
    case file:write_file(Fname, OutConf) of
        ok -> {ok, Fname};
        {error, Reason} -> {error, Reason}
    end.


