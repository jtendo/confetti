Confetti
========

Confetti is configuration provider for your Erlang applications.

Basically it's `application:get_env/2` on steroids.

Features
--------

### Management console

* Accessible via telnet - maintenance department **will** love you for this
* Configuration reload in runtime (designated processes receive notifications on reload)
* Easily extensible with your own management commands (plugins!)
* (TODO) broadcast working configuration across the Erlang cluster

![Confetti management console](http://mtod.org/assets/c3/w92p4radk4wg8.png)

### Configuration supervision

* Increase your system's uptime - previous working configuration is
  DETS-cached in case someone messes up the configuration files
* Broken config for ``process_a`` can not break ``process_b``

![Confetti supervision tree](http://mtod.org/assets/83/n4jtwvai8s4ck.png)

### Easy to use

```erlang
application:start(confetti).
```
then

```erlang
%% your process
%% (...)
init([]) ->
    confetti:use(my_foo),   %% reads configuration terms
                            %% from "conf/my_foo.conf",
                            %% spawns new configuration provider
                            %% if needed...

    confetti:fetch(my_foo),  %% fetches the configuration terms
    {ok, #state{}}.

%% (...)
%% react to configuration changes
handle_info({config_reloaded, NewConf}, State) -> (...)
```

### Customizable

* Write configuration validators and more:

```erlang
confetti:use(foo, [
    %% Specify config file location
    {location, {"conf/bar", "foo.cnf"},

    %% Make sure it's more than just correct Erlang term
    %% or even transform the terms into something!
    %% Validator funs should accept Config and return {ok, NewConf}
    %% on success, error otherwise.
    {validators, [fun validate_foo_config/1]},

    %% ignore notifications for current process
    {subscribe, false}
]).
```

* Expose any module via the management console:

```erlang
-module(my_commands).
export([foo/1, foo/3]).

foo(help) ->
    "Foo does bar two times!".
foo(Param1, Param2, Param3) ->
    %% perform command logic
    "bar bar".
```

Let confetti know about it:

```erlang
%% conf/mgmt_conf.conf
{port, 50000}.
{plugins, [my_commands]}.
```

Assuming your application is already running,
perform live management configruation reload:

```
$ telnet localhost 50000

...

(nonode@nohost)> reload mgmt_conf
ok
```

* Provide your own welcome screen to the management console, i.e.:

```
$ figlet MyApp > priv/helo.txt
```

Try it out quickly
------------------

1. Obtain the source code
2. `rebar compile; erl -pa ebin -boot start_sasl -s confetti_app`
3. `1> example_srv:start_link().`
3. `telnet localhost 50000`
4. Type `help` for available commands, and `help COMMAND` for command usage
   details.


License
-------

BSD License.
See `LICENSE` file for details.


Authors
-------
Adam Rutkowski `<adam.rutkowski@jtendo.com>`


Contribute!
-----------
Feel encouraged to spot bugs/poor code and implement new sexy features.

Also, make sure, you add yourself to the ``authors`` where appropriate!
Thanks.


