-module(confetti_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

% useful for console starts
% i.e. erl -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -s confetti_app
start() ->
    application:start(confetti).

start(_StartType, _StartArgs) ->
    confetti_sup_sup:start_link().

stop(_State) ->
    ok.
