%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Application main supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_sup_sup).
-author('adam.rutkowski@jtendo.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
        ?CHILD(confetti_table_man_sup, supervisor),
        ?CHILD(confetti_sup, supervisor),
        ?CHILD(confetti_mgmt_sup, supervisor)
    ],
    {ok, { {one_for_one, 5, 10}, Children} }.

