Confetti
========

**This is just a draft at experimental stage.**

The idea:
---------

1. If you can perform code hot-swap, you should do well with config
2. Assumption: config is Erlang term(s) and application:get_env/2 is rough
3. Works within multinode cluster; edit config on one node, push it to the
   others, have local backups automatically created on successful reload
4. Provides shell for management, easily extensible with user-commands

    ❯ telnet localhost 9991
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.

                           ____     __  __  _
         _________  ____  / __/__  / /_/ /_(_)
        / ___/ __ \/ __ \/ /_/ _ \/ __/ __/ /
       / /__/ /_/ / / / / __/  __/ /_/ /_/ /
       \___/\____/_/ /_/_/  \___/\__/\__/_/

             Type `help` for assistance.
                Press Ctrl+D to quit.

    (confetti)> 

5. Works well with Unicode
6. Write your own in/out hooks for config processing (i.e. have config dumps annotated)
7. Register as many config providers as you like - no broken config for Mod1 can break Mod2


Status:
-------

This a weekend pet project of mine. Unfinished and probably ugly here & there.
I will probably take care of it someday.
Also, I had something similar working on production, but you know how it is:

http://blogs.msdn.com/cfs-file.ashx/__key/CommunityServer-Blogs-Components-WeblogFiles/00-00-01-32-02-metablogapi/8054.image_5F00_thumb_5F00_35C6E986.png



