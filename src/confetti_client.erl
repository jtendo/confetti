-module(confetti_client).
-author('adam.rutkowski@jtendo.com').
-export([behaviour_info/1, start/1, start/2, start/3, start/4]).
-include("confetti.hrl").

behaviour_info(callbacks) ->
    [
        {process_config_in, 1},
        {process_config_out, 2},
        {handle_event, 2}
    ];

behaviour_info(_) ->
    undefined.

start(ProviderName) ->
    confetti:start_link(ProviderName, #confetti_opts{
            filename = ProviderName ++ ".conf"
        }).

start(ProviderName, Filename) ->
    confetti:start_link(ProviderName, #confetti_opts{
            filename = Filename
        }).

start(ProviderName, Directory, Filename) ->
    confetti:start_link(ProviderName, #confetti_opts{
            filename = Filename,
            directory = Directory
        }).

start(ProviderName, Directory, Filename, CallbackModule) ->
    confetti:start_link(ProviderName, #confetti_opts{
            filename = Filename,
            directory = Directory,
            callback_module = CallbackModule
        }).
