%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo/
%%% @doc
%%%
%%% @end
%%% Created : 2011-07-10 12:57:37.244363
%%%-------------------------------------------------------------------
-module(confetti).
-behaviour(gen_server).
-include("confetti.hrl").

%% API
-export([start_link/2]).
-export([fetch/1, reload/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

reload(ProviderName) ->
    gen_server:call(ProviderName, {reload_config}).

fetch(ProviderName) ->
    gen_server:call(ProviderName, {fetch_config}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ProviderName, Opts = #confetti_opts{}) when is_atom(ProviderName) ->
    gen_server:start_link({local, ProviderName}, ?MODULE, {ProviderName, Opts}, []).

init({ProviderName, Opts}) ->
    case confetti_reader:load_config(Opts) of
        {ok, RawConf, Conf} ->
            confetti_utils:clear_alarm(ProviderName),
            {ok, #provider{opts=Opts, conf=Conf, raw_conf=RawConf}};
        Error ->
            confetti_utils:raise_alarm(ProviderName, Error),
            Error
    end.

handle_call({reload_config}, _From, State) ->
    Opts = State#provider.opts,
    Conf = State#provider.conf,
    RawConf = State#provider.raw_conf,
    case confetti_reader:load_config(Opts) of
        {ok, NewRawConf, NewConf} ->
            NewState = State#provider{raw_conf=NewRawConf, conf=NewConf},
            confetti_writer:write_config(Opts, Conf, RawConf),
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({fetch_config}, _From, State) ->
    {reply, State#provider.conf, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


