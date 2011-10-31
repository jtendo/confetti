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

use(ProviderName) ->
    use(ProviderName,
        [{location, {atom_to_list(ProviderName) ++ ".conf", "conf"}},
         {validators, []},
         {subscribe, true}
        ]).

use(ProviderName, Opts) ->
    {ok, Pid} = confetti_sup:start_child(ProviderName, Opts),
    Subscribe = proplists:get_value(subscribe, Opts, false),
    case Subscribe of
        true ->
            ok = gen_server:call(ProviderName, {subscribe, ProviderName}),
            {ok, Pid};
        _ ->
            {ok, Pid}
    end.

reload(ProviderName) ->
    case gen_server:call(ProviderName, {reload_config, ProviderName}) of
        {ok, Conf} -> notify_subscribers(ProviderName, {config_reloaded, Conf});
        Err -> Err
    end.

fetch(ProviderName) ->
    gen_server:call(ProviderName, {fetch_config}).

start_link(ProviderName, Opts) when is_atom(ProviderName) ->
    pg2:create(ProviderName),
    gen_server:start_link({local, ProviderName}, ?MODULE, {ProviderName, Opts}, []).

%%%===================================================================
%%% Gen Server Callbacks
%%%===================================================================

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

handle_call({fetch_config}, _From, State) ->
    {reply, State#provider.conf, State};

handle_call({subscribe, ProviderName}, {Pid, _}, State) ->
    ok = join_pool(ProviderName, Pid),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% just for debug purposes
handle_info(calcbad, State) ->
    1/0,
    {noreply, State};

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
join_pool(Pool, Pid) ->
    case lists:member(Pid, pg2:get_local_members(Pool)) of
        true -> ok;
        false ->
            pg2:join(Pool, Pid)
    end.

notify_subscribers(Pool, Msg) ->
    lists:foreach(fun(Pid) ->
                Pid ! Msg
        end, pg2:get_local_members(Pool)).
