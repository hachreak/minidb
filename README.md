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

```erlang
2> riak_core:join('test2@127.0.0.1').
```

Ping
----

Test the ping (you can run from both nodes):

```erlang
3> riak_core_demo:ping().
```

You should see messages from console like:

`{pong, ....}`

and in one of the two nodes (it's pseudo random) a message like:

`[ping received] from ...`

Get/Put values in the store
---------------------------

```erlang
riak_core_demo:put(a, 1).
riak_core_demo:put(b, 2).
...
riak_core_demo:get(a).
riak_core_demo:get(b).
```

Handoff
-------

After you insert many data in the storeTest the store, just try to disconnect
the second node from the cluster:

```erlang
riak_core:leave().
```

In the console you should be able to see the different messages.
Automatically, all information stored in the second node will be transfered
to the first node.

To reconnect again the second node, simply:

```erlang
application:ensure_all_started(riak_core_demo).
riak_core:join('test1@127.0.0.1').
```

Again, you will see the handoff of some process from the first node and
part of the data will be transfered to the second node.
