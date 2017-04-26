riak_core_demo
=====

An OTP application

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
riak_core:join('test2@127.0.0.1').
```

Test the ping:

```bash
riak_core_demo:ping().
```

You should see messages from console like:

`{pong, ....}`

and in one of the two nodes (it's pseudo random) a message like:

`[ping received] from ...`
