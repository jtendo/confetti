-module(example_user_commands).

-export([say_hi/0, say_hi/1]).

say_hi(help) ->
    "This commands greets you!".

say_hi() ->
    "Oh Hai!".
