-module(confetti_utils).
-author('adam.rutkowski@jtendo.com').
-export([u_consult/1, fname/1, fname/2]).
-export([raise_alarm/2, clear_alarm/1]).
-include("confetti.hrl").


%%%===================================================================
%%% API
%%%===================================================================

fname(normal, Opts = #confetti_opts{}) ->
    Directory = Opts#confetti_opts.directory,
    Filename = Opts#confetti_opts.filename,
    ConfigFile = filename:join(Directory, Filename),
    ConfigFile;

fname(dump, Opts = #confetti_opts{}) ->
    Directory = Opts#confetti_opts.directory,
    Filename = Opts#confetti_opts.filename ++ ".dump",
    ConfigFile = filename:join(Directory, Filename),
    ConfigFile.

fname(Opts = #confetti_opts{}) ->
    fname(normal, Opts).

% same as consult, except for encoding flag
u_consult(File) ->
    case file:open(File, [read, {encoding, unicode}]) of
        {ok, Fd} ->
            R = consult_stream(Fd),
            file:close(Fd),
            R;
        Error ->
            Error
    end.

%%%===================================================================
%%% API (alarms)
%%%===================================================================

raise_alarm(ProviderName, AlarmDesc) ->
    case application:get_application(sasl) of
        undefined ->
            io:format("Alarm! Could not start ~p!~n", [ProviderName]);
        {ok, sasl} ->
            alarm_handler:set_alarm({{confetti, ProviderName}, AlarmDesc})
    end.

clear_alarm(ProviderName) ->
    case application:get_application(sasl) of
        {ok, sasl} ->
            alarm_handler:clear_alarm({confetti, ProviderName});
        _ -> ok
    end.


%%%===================================================================
%%% Helpers
%%%===================================================================

consult_stream(Fd) ->
    consult_stream(Fd, 1, []).

consult_stream(Fd, Line, Acc) ->
    case io:read(Fd, '', Line) of
        {ok,Term,EndLine} ->
            consult_stream(Fd, EndLine, [Term|Acc]);
        {error, Error, _Line} ->
            {error, Error};
        {eof,_Line} ->
            {ok,lists:reverse(Acc)}
    end.



