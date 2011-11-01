%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Configuration providers supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_sup).
-behaviour(supervisor).

%-export([start_link/0, start/1, start/2, start/3, start/4]).
-export([start_link/0,init/1]).
-export([start_child/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 60, 3600},
         [{conf_server,
          {confetti, start_link, []},
          transient, 1000, worker, [confetti]}
         ]}}.


start_child(ProviderName, Options) ->
    case supervisor:start_child(?MODULE, [ProviderName, Options]) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.
