Confetti
========

**PLEASE NOTE: This is just a draft at experimental stage, there's still a
bunch of things to implement.**

Confetti is configuration provider for your Erlang applications.

Basically it's `application:get_env/2` on steroids.

Features
--------

1. Management console (accessible via telnet) - maintenance department **will** love
you for this:

    * configuration reload in runtime (your processes receive notifications on reload)
    * easily extensible with your own management commands (plugins!)
    * broadcast working configuration across the Erlang cluster

   ![Confetti management console](http://mtod.org/assets/75/blnil7js4k0k4.png)

2. Configuration supervision:

   * increase your system's uptime - previous working configuration is backed
     up in DETS, in case someone messes up the configuration files
   * broken config for ``process_a`` can not break ``process_b``

   ![Confetti supervision tree](http://mtod.org/assets/83/n4jtwvai8s4ck.png)

3. Easy to use

    ```erlang
    %% your process
    %% (...)
    confetti:use(my_foo),   %% reads configuration terms
                            %% from "conf/my_foo.conf"
    confetti:fetch(my_foo)  %% fetches the configuration terms
    %% (...)
    %% react to configuration changes
    handle_info({config_reloaded, NewConf}, State) -> (...)
    ```

4. Customizable

    * Write configuration validators and more:

    ```erlang
    confetti:use(foo, [
        %% specify config file location
        {location, {"conf/bar", "foo.cnf"},
        %% make sure it's more than just correct Erlang term
        %% or even transform the terms into something!
        {validators, [fun validate_foo_config/1]},
        %% ignore notifications for current process
        {subscribe, false}
    ]).
    ```

    * Expose any module via the management console

    ```erlang
    %% mgmt_conf.conf (confetti uses confetti, so meta!)
    {port, 50000}.
    {plugins, [my_commands]}.
    ```

    ```erlang
    %% my_commands.erl
    %% (...)
    export([foo/1, foo/3]).
    foo(help) ->
        "Foo does bar two times!".
    foo(Param1, Param2, Param3) ->
        %% perform command logic
        "bar bar".
    ```

    * Provide your own welcome screen to the management console.


License
-------

BSD License.


Contribute!
-----------
Feel encouraged to spot bugs/poor code and implement new sexy features.
Also, remember to add yourself to the ``-authors`` where appropriate!
Thanks.
