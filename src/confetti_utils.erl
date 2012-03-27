%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Confetti utlility module
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_utils).
-author('adam.rutkowski@jtendo.com').
-export([u_consult/1, fname/1, fname/2, ensure_proplist/1]).
-export([raise_alarm/2, clear_alarm/1]).
-include("confetti.hrl").


%%%===================================================================
%%% API
%%%===================================================================

fname(normal, {Filename, Directory}) ->
    ConfigFile = filename:join(Directory, Filename),
    ConfigFile;

fname(dump, {Filename, Directory}) ->
    Marker = "_" ++ f_date(erlang:localtime()) ++ ".dump",
    ConfigFile = filename:join([Directory, "dump", Filename ++ Marker]),
    ConfigFile.

fname(Location = {_, _}) ->
    fname(normal, Location).

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

%% names speaks for itself
ensure_proplist([]) -> true;
ensure_proplist([Term|Terms]) ->
    case Term of
        {_K, _V} -> ensure_proplist(Terms);
        _ -> false
    end;
ensure_proplist(_) -> false.



%%%===================================================================
%%% API (alarms)
%%%===================================================================

raise_alarm(ProviderName, AlarmDesc) ->
    case application:get_application(sasl) of
        undefined ->
            io:format("Alarm! Could not start ~p!~n", [ProviderName]);
        {ok, sasl} ->
            Alarms = alarm_handler:get_alarms(),
            case proplists:get_value({confetti, ProviderName}, Alarms) of
                undefined ->
                    alarm_handler:set_alarm({{confetti, ProviderName}, AlarmDesc});
                _ ->
                    ok
            end
    end.

clear_alarm(ProviderName) ->
    case application:get_application(sasl) of
        {ok, sasl} ->
            Alarms = alarm_handler:get_alarms(),
            case proplists:get_value({confetti, ProviderName}, Alarms) of
                undefined -> ok;
                _ ->
                    alarm_handler:clear_alarm({confetti, ProviderName})
            end;
        _ -> ok
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

f_date(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B~2.10.0B~2.10.0B_~2.10.0B_~2.10.0B_~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

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
