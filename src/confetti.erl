%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Confetti main module
%%% @end
%%%-------------------------------------------------------------------
-module(confetti).
-behaviour(gen_server).
-include("confetti.hrl").

%% API
-export([start_link/2]).
-export([use/1, use/2]).
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

-type opts() :: [
    {location, {Filename :: string(), Directory :: string()}} |
    {validators, [fun( (Conf :: term()) ->
                        {ok, NewConf :: term()} | {error, Reason :: any()} )]} |
    {subscribe, boolean()}
].

-spec use(ProviderName :: atom()) -> {ok, Pid :: pid()}.


%% @doc
%% Obtains configuration provider process with default settings.
%% Reads configuration terms from "conf/ProviderName.conf".
%% Subscribes caller to configuration reload notification.

use(ProviderName) ->
    use(ProviderName,
        [{location, {atom_to_list(ProviderName) ++ ".conf", "conf"}},
         {validators, []},
         {subscribe, true}
        ]).

-spec use(ProviderName :: atom(), Opts :: opts()) -> {ok, Pid :: pid()}.

%% @doc
%% Obtains configuration provider process
%% @see opts()

use(ProviderName, Opts) ->
    {ok, Pid} = confetti_sup:start_child(ProviderName, Opts),
    Subscribe = proplists:get_value(subscribe, Opts, true),
    case Subscribe of
        false ->
            {ok, Pid};
        _ ->
            ok = gen_server:call(ProviderName, {subscribe, ProviderName}),
            {ok, Pid}
    end.

-spec reload(ProviderName :: atom()) -> ok.

%% @doc
%% Reloads given provider's configuration from disk.
%% Notifies subscribers with {config_reloaded, Conf} message on success.
%% This function is not intented to be called directly.
%% Confetti uses it via management console commands.

reload(ProviderName) ->
    case is_provider(ProviderName) of
        true ->
            case gen_server:call(ProviderName, {reload_config, ProviderName}) of
                {ok, Conf} -> notify_subscribers(ProviderName, {config_reloaded, Conf});
                Err -> Err
            end;
        false -> not_provider(ProviderName)
    end.

-spec fetch(ProviderName :: atom()) -> Conf :: term().

%% @doc
%% Fetch configuration terms from given provider process.
%% User is responsible for implementing detailed
%% config value getters.

fetch(ProviderName) ->
    case is_provider(ProviderName) of
        true -> gen_server:call(ProviderName, {fetch_config});
        false -> not_provider(ProviderName)
    end.

%% @doc
%% Starts the server, and pg2 group if needed.
%% This function is called by confetti_sup module and
%% there is probably no need to call it directly.

start_link(ProviderName, Opts) when is_atom(ProviderName) ->
    pg2:create(ProviderName),
    gen_server:start_link({local, ProviderName}, ?MODULE, {ProviderName, Opts}, []).

%%%===================================================================
%%% Gen Server Callbacks
%%%===================================================================


%% @doc
%% Tries to load configuration for given provider.
%% If disk configuration loading fails (i.e. config terms are broken,
%% or configuration file does not exists), init/1 will try
%% to fetch the last working configuration from DETS.

init(Subject = {ProviderName, Opts}) ->
    case confetti_reader:load_config(Subject) of
        {ok, RawConf, Conf} ->
            {ok, #provider{opts=Opts, conf=Conf, raw_conf=RawConf}};
        Error ->
            case confetti_reader:last_working_config(ProviderName) of
                {ok, {Opts, PrevRawConf, PrevConf}} ->
                    {ok, #provider{opts=Opts, conf=PrevConf, raw_conf=PrevRawConf}};
                _Else -> Error
            end
    end.

%% @private
%% @doc
%% Reload configuration for given provider.
%% Dump previous configuration to disk on success.

handle_call({reload_config, ProviderName}, _From, State) ->
    Opts = State#provider.opts,
    case confetti_reader:load_config({ProviderName, Opts}) of
        {ok, NewRawConf, NewConf} ->
            NewState = State#provider{raw_conf=NewRawConf, conf=NewConf},
            Conf = State#provider.conf,
            RawConf = State#provider.raw_conf,
            confetti_writer:dump_config(proplists:get_value(location, Opts), Conf, RawConf),
            {reply, {ok, NewConf}, NewState};
        Error ->
            {reply, Error, State}
    end;

%% @private
%% @doc
%% Fetches the current provider's configuration from State.

handle_call({fetch_config}, _From, State) ->
    {reply, State#provider.conf, State};

%% @private
%% @doc
%% Join subscribers group if not there already.

handle_call({subscribe, ProviderName}, {Pid, _}, State) ->
    ok = join_group(ProviderName, Pid),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% just for debug purposes
%handle_info(calcbad, State) ->
    %1/0,
    %{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% don't know why pg2:join doesn't do it by default
join_group(Group, Pid) ->
    case lists:member(Pid, pg2:get_local_members(Group)) of
        true -> ok;
        false ->
            pg2:join(Group, Pid)
    end.

notify_subscribers(Group, Msg) ->
    lists:foreach(fun(Pid) ->
                Pid ! Msg
        end, pg2:get_local_members(Group)),
    ok.

is_provider(ProviderName) ->
    case whereis(ProviderName) of
        Pid when is_pid(Pid) ->
            Providers = supervisor:which_children(confetti_sup),
            lists:member(Pid, [P || {_,P,_,_} <- Providers]);
        _ -> false
    end.

not_provider(ProviderName) ->
    throw({unknown_provider, ProviderName}).
