-module(confetti_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 60, 3600},
         [{conf_server,
          {confetti, start_link, []},
          transient, 1000, worker, [confetti]}
         ]}}.

start_child(ProviderName, Options) ->
    supervisor:start_child(?MODULE, [ProviderName, Options]).

