%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% DETS Manager
%%% @end
%%%-------------------------------------------------------------------
-module(confetti_table_man).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([store/1, lookup/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    dets:open_file(?SERVER, [{type, set}]).

store(Data) ->
    gen_server:call(?SERVER, {store, Data}).

lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

handle_call({store, Data}, _From, State) ->
    Reply = dets:insert(?SERVER, Data),
    {reply, Reply, State};

handle_call({lookup, Key}, _From, State) ->
    Reply = dets:lookup(?SERVER, Key),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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




