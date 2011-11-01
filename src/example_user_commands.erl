%%%-------------------------------------------------------------------
%%% @author Adam Rutkowski
%%% @copyright (C) 2011, jtendo
%%% @doc
%%% Example of management console plugin module.
%%% @end
%%%-------------------------------------------------------------------
-module(example_user_commands).

-export([say_hi/0, say_hi/1]).

say_hi(help) ->
    "This commands greets you!".

say_hi() ->
    "Oh Hai!".
