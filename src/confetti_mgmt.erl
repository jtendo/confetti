-module(confetti_mgmt).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("confetti.hrl").

-record(state, {socket}). % the current socket

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   API                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Socket) ->
    code:ensure_loaded(confetti_mgmt_cmnds),
    gen_server:start_link(?MODULE, Socket, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          gen_server callbacks                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Socket) ->
    gen_server:cast(self(), accept),
    confetti:use(mgmt_conf),
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
    "Usage: help COMMAND";

handle_command(["help"|Topic]) ->
    ErrMsg = io_lib:format("No help for ~s", [hd(Topic)]),
    try_execute(hd(Topic), [help], [
            {export_err, ErrMsg},
            {except_err, ErrMsg}
        ]);

handle_command([Cmd|Params]) ->
    try_execute(Cmd, Params).

try_execute(F, A) ->
    try_execute(F, A, [
            {export_err, "Unknown command or syntax error"},
            {except_err, "Unknown command"}
        ]).

try_execute(F, A, ErrMsgs) ->
    CommandsModule = ?FETCH(mgmt_conf, cmd_module),
    try list_to_existing_atom(F) of
         Func when is_atom(Func) ->
            case erlang:function_exported(CommandsModule, Func,
                    length(A)) of
                true ->
                    try apply(confetti_mgmt_cmnds, Func, A) of
                        Result -> Result
                        catch Class:Error ->
                            io_lib:format("Error (~p): ~p", [Class, Error])
                    end;
                false ->
                    proplists:get_value(export_err, ErrMsgs)
            end
        catch _Class:_Error ->
            proplists:get_value(except_err, ErrMsgs)
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
    gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    inet:setopts(Socket, [{active, once}]),
    ok.

cmd(Str) when is_list(Str) ->
    string:tokens(hd(string:tokens(Str, "\r\n")), " ").

