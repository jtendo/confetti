-record(confetti_opts, {
        directory="conf",
        filename,
        callback_module
    }).

-record(provider, {
        opts,
        raw_conf,
        conf
    }).

