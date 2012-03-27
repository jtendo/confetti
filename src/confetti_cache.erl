%%%-------------------------------------------------------------------
%%% @author Dmitry Groshev
%%% @copyright (C) 2012, Selectel
%%% @doc
%%% Confetti cache module
%%% @end
%%%-------------------------------------------------------------------

-module(confetti_cache).

%% API
-export([write/1, read/0, get/1, get_many/1, get_many/2]).

-spec write([{any(), any()}]) -> ok.

%% @doc
%% Writes an entire configuration (which should be a proplist) to cache.

write(Lst) when is_list(Lst) ->
    case confetti_utils:ensure_proplist(Lst) of
        true -> ok;
        false -> throw("only proplist configs are cacheable")
    end,
    [erlang:put({confetti_cache, K}, V) || {K, V} <- Lst],
    ok.

-spec read() -> [{any(), any()}].

%% @doc
%% Reads an entire configureation from cache.

read() ->
    [{K, V} || {{confetti_cache, K}, V} <- erlang:get()].

-spec get(any()) -> any() | none.

%% @doc
%% Reads configuration value from cache by key. If key wasn't found,
%% returns none.

get(K) ->
    case erlang:get({confetti_cache, K}) of
        undefined -> none;
        X -> X
    end.

-spec get_many([any()] | [{any(), any()}]) -> [any()].

%% @doc
%% Reads a list of configuration values from cache by list of keys. If some
%% keys weren't found, returns none instead of that keys.
%% @see get_many/2

get_many(Ks) ->
    get_many(Ks, nothrow).

-spec get_many([any()] | [{any(), any()}], throw|nothrow) -> [any()].

%% @doc
%% Reads a list of configuration values from cache by list of keys. Behaviour
%% in the case of missing key depends on second argument - if it is equal to
%% 'throw', throws key_not_found exception; if it is equal to 'nothrow',
%% behaves similarly to get_many/1.

get_many(Ks, ThrowOrNot) ->
    [case erlang:get({confetti_cache, K}) of
         undefined -> none_or_throw(K, ThrowOrNot);
         X -> X
     end || K <- Ks].

%%%===================================================================
%%% Internal functions
%%%===================================================================

none_or_throw(K, throw) -> throw({key_not_found, K});
none_or_throw(_, nothrow) -> none.
