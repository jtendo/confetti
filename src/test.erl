%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo/
%%% @doc
%%%
%%% @end
%%% Created : 2011-10-26 10:14:13.101666
%%%-------------------------------------------------------------------
-module(test).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

validate1(Conf) ->
    io:format("Config seems valid!~n"),
    io:format("Operating on ~p~n", [Conf]),
    {ok, {modified_conf, Conf}}.

validate2(Conf = {modified_conf, Data}) ->
    io:format("Another validator!~n"),
    io:format("Operating on ~p~n", [Conf]),
    {ok, {transofmed_conf, Conf}}.

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    confetti:start(example, ?SERVER, [
            {location, {"example.conf", "conf"}},
            {validators, [fun validate1/1, fun validate2/1]}
        ]),
    {ok, Pid}.

init([]) ->
    {ok, 1}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("confetti client got info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




