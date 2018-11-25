minidb
==============

This is a minimal in-memory distributed master-less document database database.

API
---

#### Cluster:

- Auto-cluster: automatically create a cluster specified in the configuration
    `minidb_cluster:join()`, or manually make a cluster passing a list of
    seeds `minidb_cluster:make([bootstrap_node1])`.
- Cluster status: `minidb_cluster:status()`
- Join a cluster: `minidb_cluster:join('bootstrap_node')`
- Leave a cluster: `minidb_cluster:leave()`
- Ping a random node: `minidb:ping()`

#### CRUD:

- Create/Update a key: `minidb:put(key, value)`
- Patch a value: `minidb:patch(key, {subkey1, subvalue1})`
- Read a value: `minidb:get(key)` or `minidb:get(key, default)`
- Query database: `minidb:find([..])`
- Get all keys: `minidb:keys()`
- Delete a key: `minidb:delete`
- Drop everything: `minidb:drop()`

#### Query:

The query language is similar to the one used by mongodb:

`minidb:find([{"section", {'$eq', "books"}}, {"likes", {'$gt', 100}}])`

Operators available:

Op.           | Name              | Example
--------------|-------------------|------------------------------------------
`'$eq'`       | Equals            | {"section", {'$eq', "books"}}
`'$ne'`       | Not Equals        | {"section", {'$ne', "books"}}
`'$gt'`       | Greater Than      | {"likes", {'$gt', 100}}
`'$gte'`      | Greater or Equals | {"likes", {'$gte', 100}}
`'$lt'`       | Lower Than        | {"likes", {'$lt', 100}}
`'$lte'`      | Lower or Equals   | {"likes", {'$lte', 100}}

Note: to get everything: `minidb:find([])`.

Build
-----

    $ rebar3 compile

Run demo
--------

Start the first node:

    $ make node1

Start the second node:

    $ make node2

After 5 seconds, the second node will automatically connect to the first in a
cluster.

To clean all the configuration (cluster informations):

    $ make clean
