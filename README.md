minidb
==============

This is a minimal in-memory distributed master-less document database database.

Build
-----

    $ rebar3 compile

Run demo
--------

Start the first node:

    $ make node1

Start the second node:

    $ make node2
    > minidb:join('test1@127.0.0.1').

The second node will automatically connect to the first in a cluster.

Ping
----

Test the ping (you can run from both nodes):

```erlang
3> minidb:ping().
```

You should see messages from console like:

`{pong, ....}`

and in one of the two nodes (it's pseudo random) a message like:

`[ping received] from ...`

Get/Put values in the store
---------------------------

```erlang
minidb:put(a, 1).
minidb:put(b, 2).
...
minidb:get(a).
minidb:get(b).
```

Handoff
-------

After you insert many data in the store, just try to disconnect
the second node from the cluster:

```erlang
riak_core:leave().
```

In the console you should be able to see the different messages.
Automatically, all information stored in the second node will be transfered
to the first node.

To reconnect again the second node, simply:

```erlang
application:ensure_all_started(minidb).
riak_core:join('test1@127.0.0.1').
```

Again, you will see the handoff of some process from the first node and
part of the data will be transfered to the second node.


Coverage commands: keys() and values()
--------------------------------------

After you insert many data in the store, try to get the list of keys stored in
all nodes or list of values:

```erlang
minidb:keys().
minidb:values().
```

Cluster status
--------------

Show cluster status:

```erlang
minidb:status().
```
