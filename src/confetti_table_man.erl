%% i could probably think of having a state of art table manager
%% as described here:
%% http://www.planeterlang.org/en/planet/article/Dont_Lose_Your_ets_Tables/
%% nevertheless a public set should be sufficent, as long as we keep it
%% owned by a process that doesn't really do much except for creating the table

-module(confetti_table_man).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([create/1]).

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

create(TableName) ->
    gen_server:call(?SERVER, {create, TableName}).

init([]) ->
    case ets:info(confetti) of
        undefined -> ets:new(confetti, [set, public, named_table]);
        confetti -> confetti
    end,
    {ok, confetti}.

%handle_call({create, TableName}, _F, State) ->
%    case ets:info(TableName) of
%        undefined -> ets:new(TableName, [set, public, named_table]);
%        TableData -> TableName
%    end,
%    {reply, TableName, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%handle_info({'ETS-TRANSFER', TableId, OldOwner, HeirData}) ->
%    io:format("OldOwner dies ~p~n", [OldOwner]),
%    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




