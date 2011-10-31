-record(provider, {
        opts,
        raw_conf,
        conf
    }).

-define(FETCH, fun(Provider, Key) ->
            proplists:get_value(Key, confetti:fetch(Provider))
    end).

-define(SOCK(Msg), {tcp, _Port, Msg}).

-define(HELO,
            fun() ->
                    case code:priv_dir(confetti) of
                        {error, bad_name} -> HeloFile = "priv/helo.txt";
                        Dir -> HeloFile = filename:join(Dir, "helo.txt")
                    end,
                    case filelib:is_file(HeloFile) of
                        true ->
                            {ok, Helo} = file:read_file(HeloFile),
                            binary_to_list(Helo);
                        false ->
                            "Could not read helofile."
                    end
            end).

-define(PROMPT,
            fun() ->
                    io_lib:format("(~w)> ", [node()])
            end).
