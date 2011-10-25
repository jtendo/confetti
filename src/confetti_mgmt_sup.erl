%%% The supervisor in charge of all the socket acceptors.
-module(confetti_mgmt_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    start_socket(),
    {ok, Pid}.

init([]) ->
    Port = 50000,
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once},
            {reuseaddr, true}]),
    {ok, {{simple_one_for_one, 60, 3600},
         [{socket,
          {confetti_mgmt, start_link, [ListenSocket]}, % pass the socket!
          temporary, 1000, worker, [confetti_mgmt]}
         ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

