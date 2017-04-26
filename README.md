riak_core_demo
==============

This is a riak demo application following the elixir tutorials by
[Gpad](https://github.com/gpad/no_slides) and adapting them to erlang

([Part 1](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-1-41354c1f26c3),
[Part 2](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-2-88bdec73f368),
[Part 3](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-3-8bac36632be0),
[Part 4](https://medium.com/@GPad/create-a-riak-core-application-in-elixir-part-4-728512ece224))

Build
-----

    $ rebar3 compile

Run demo
--------

Start the first node:

```bash
rebar3 shell --name test1@127.0.0.1 --config config/vars_dev1.config
1> application:ensure_all_started(riak_core_demo).
```

Start the second node:

```bash
rebar3 shell --name test2@127.0.0.1 --config config/vars_dev2.config
1> application:ensure_all_started(riak_core_demo).
```

Join the two nodes together (run from the first node):

```bash
2> riak_core:join('test2@127.0.0.1').
```

Test the ping (you can run from both nodes):

```bash
3> riak_core_demo:ping().
```

You should see messages from console like:

`{pong, ....}`

and in one of the two nodes (it's pseudo random) a message like:

`[ping received] from ...`
