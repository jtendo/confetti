-module(confetti_writer).
-author('adam.rutkowski@jtendo.com').
-export([write_config/3]).
-include("confetti.hrl").

%%%===================================================================
%%% API
%%%===================================================================

write_config(Opts, Conf, RawConf) ->
    case Opts#confetti_opts.callback_module of
        undefined ->
            Result = dump(Opts, RawConf);
        Module ->
            case Module:process_config_out(RawConf, Conf) of
                {ok, OutConf} ->
                    Result = dump(Opts, OutConf);
                {error, Reason} ->
                    Result = {error, Reason}
            end
    end,
    Result.

%%%===================================================================
%%% Helpers
%%%===================================================================

dump(Opts, OutConf) ->
    Fname = confetti_utils:fname(dump, Opts),
    case file:write_file(Fname, OutConf) of
        ok -> {ok, Fname};
        {error, Reason} -> {error, Reason}
    end.


