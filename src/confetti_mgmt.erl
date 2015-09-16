%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Management console socket server and function execution engine
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_mgmt).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-compile([export_all]).
-include("confetti.hrl").

-record(state, {socket}). % the current socket

% functions that should not be treated as management commands
-define(PRIVATE_INTERFACE, [module_info]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          gen_server callbacks                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Socket) ->
    gen_server:cast(self(), accept),
    confetti:use(mgmt_conf),
    lists:foreach(fun(M) -> {module, M} = code:ensure_loaded(M) end,
        all_cmd_modules()),
    {ok, #state{socket=Socket}}.

handle_call(_, _, State) ->
    {noreply, State}.

%% Accepting a connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    confetti_mgmt_sup:start_socket(),
    welcome(AcceptSocket),
    {noreply, S#state{socket=AcceptSocket}};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(?SOCK([4]), S = #state{socket=Socket}) ->
    send(Socket, "~nBye!", []),
    {stop, normal, S};

handle_info(?SOCK("\r\n"), S = #state{socket=Socket}) ->
    prompt(Socket),
    {noreply, S};

handle_info(?SOCK(E), S = #state{socket=Socket}) ->
    send(Socket, "~ts", [handle_command(cmd(E))]),
    prompt(Socket),
    {noreply, S};

handle_info({tcp_closed, _}, S) ->
    {stop, normal, S};

handle_info(_E,S) ->
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           management handlers                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_command(["help"]) ->
    AvailableCommands = lists:map(fun get_plugin_help/1, all_cmd_modules()),
    string:join(AvailableCommands, "\n");

handle_command(["help"|Topic]) ->
    ErrMsg = io_lib:format("No help for ~s", [hd(Topic)]),
    try_execute(hd(Topic), [help], ErrMsg);

handle_command([Cmd|Params]) ->
    try_execute(Cmd, Params).

find_cmd_module({_, _}, []) ->
    {error, undefined};

find_cmd_module({F, Arity}, [M|Rest]) ->
    case erlang:function_exported(M, F, Arity) of
        true ->
            {found, {M, F}};
        false ->
            find_cmd_module({F, Arity}, Rest)
    end.

find_cmd_module({Cmd, Arity}) ->
    try
        F = list_to_existing_atom(Cmd),
        find_cmd_module({F, Arity}, all_cmd_modules())
    catch _:_ ->
        {error, undefined}
    end.

try_execute(F, A) ->
    try_execute(F, A, "Unknown command or syntax error").

try_execute(F, A, ErrMsg) ->
    case find_cmd_module({F, length(A)}) of
        {error, undefined} ->
             ErrMsg;
        {found, {Mod, Fun}} ->
            try apply(Mod, Fun, A) of
                Result -> Result
            catch Class:Error ->
                io_lib:format("Error (~p): ~p", [Class, Error])
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            helper functions                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prompt(Socket) ->
    gen_tcp:send(Socket, ?PROMPT()),
    inet:setopts(Socket, [{active, once}]),
    ok.

welcome(Socket) ->
    gen_tcp:send(Socket, ?HELO()),
    gen_tcp:send(Socket, ?PROMPT()),
    inet:setopts(Socket, [{active, once}]),
    ok.

send(Socket, Str, Args) ->
    try io_lib:format(Str++"~n", Args) of
        FormattedResult ->
            gen_tcp:send(Socket, FormattedResult)
    catch _:_ ->
            gen_tcp:send(Socket,
                io_lib:format("Error: Could not format command output~n", []))
    end,
    inet:setopts(Socket, [{active, once}]),
    ok.

cmd(Str) when is_list(Str) ->
    string:tokens(hd(string:tokens(Str, "\r\n")), " ").

all_cmd_modules() ->
    [confetti_mgmt_cmnds|?FETCH(mgmt_conf, plugins, [])].

get_plugin_help(Module) ->
    Exports = proplists:get_value(exports, Module:module_info(), []),
    UExports = proplists:get_keys(Exports),
    Mod = string:left(atom_to_list(Module), 30) ++ ":",
    lists:foldl(fun(F, Acc) ->
                    case lists:member(F, ?PRIVATE_INTERFACE) of
                        true -> Acc;
                        false -> string:join([Acc, atom_to_list(F)], " ")
                    end
                end, Mod, UExports).
