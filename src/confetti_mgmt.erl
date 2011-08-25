-module(confetti_mgmt).
-author('adam.rutkowski@jtendo.com').
-export([start_link/0, server/0]).

-define(PROMPT, <<"(confetti)> ">>).
-define(HELO,
<<"
                       ____     __  __  _
     _________  ____  / __/__  / /_/ /_(_)
    / ___/ __ \\/ __ \\/ /_/ _ \\/ __/ __/ /
   / /__/ /_/ / / / / __/  __/ /_/ /_/ /
   \\___/\\____/_/ /_/_/  \\___/\\__/\\__/_/

         Type `help` for assistance.
            Press Ctrl+D to quit.

(confetti)> ">>).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    Pid = spawn_link(?MODULE, server, []),
    {ok, Pid}.

server() ->
    {ok, Port} = application:get_env(confetti, mgmt_port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary,
            {active, true}, {reuseaddr, true}]),
    wait_connect(ListenSocket).

%%%===================================================================
%%% SERVER INTERNALS
%%%===================================================================

loop() ->
    receive
        {tcp, Socket, Quit} when Quit =:= <<4>>; Quit =:= <<"quit\r\n">> -> % Ctrl-d
            gen_tcp:send(Socket, "\nBye.\n"),
            gen_tcp:close(Socket),
            loop();
        {tcp_closed, _Socket} ->
            {ok, eot};
        {tcp, Socket, Bin} ->
            io:format("Got input ~p~n", [Bin]),
            Input = binary:replace(Bin, [<<"\n">>, <<"\r">>], <<"">>, [global]),
            Result = handle_input(Input),
            gen_tcp:send(Socket, Result),
            loop()
    end.

handle_input(Input) ->
    io:format("~p~n", [Input]),
    ?PROMPT.

wait_connect(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            gen_tcp:send(Socket, ?HELO),
            spawn(fun() -> wait_connect(ListenSocket) end),
            loop();
        {error, closed} ->
            ok
    end.
