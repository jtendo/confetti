-module(confetti_mgmt).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SOCK(Msg), {tcp, _Port, Msg}).

-define(HELO,
            fun() ->
                    {ok, Helo} =
                    file:read_file("priv/helo.txt"),
                    binary_to_list(Helo)
            end).

-define(PROMPT,
            fun() ->
                    io_lib:format("(~w)> ", [node()])
            end).

-record(state, {socket}). % the current socket

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          gen_server callbacks                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Socket) ->
    ok = confetti_mgmt_cmnds:init(),
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(E, _From, State) ->
    {noreply, State}.

%% Accepting a connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    confetti_mgmt_sup:start_socket(),
    welcome(AcceptSocket),
    {noreply, S#state{socket=AcceptSocket}};

handle_cast(Cast, State) ->
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

handle_command([Cmd|Params]) ->
     try list_to_existing_atom(Cmd) of
         Func when is_atom(Func) ->
                    case erlang:function_exported(confetti_mgmt_cmnds, Func, length(Params)) of
                        true ->
                            try apply(confetti_mgmt_cmnds, Func, Params) of
                                Result -> Result
                                catch Class:Error ->
                                    io_lib:format("Error (~p): ~p", [Class, Error])
                            end;
                        false ->
                            "Command not supported or parameters arity mismatch. Try: help "++Cmd
                    end
                catch _Class:_Error ->
                    "Unknown command."
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

