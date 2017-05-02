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

API
---

#### Cluster:

- Ping: `minidb:ping()`
- Join a cluster: `minidb:join('bootstrap_node')`
- Leave a cluster: `minidb:leave()`
- Cluster status: `minidb:status()`

#### CRUD:

- Create/Update a key: `minidb:put(key, value)`
- Patch a value: `minidb:patch(key, {subkey1, subvalue1})`
- Read a value: `minidb:get(key)`
- Query database: `minidb:find([..])`
- Get all keys: `minidb:keys()`
- Delete a key: `minidb:delete`
- Drop everything: `minidb:drop()`

#### Query:

The query language is similar to the one used by mongodb:

`minidb:find([{"section", {'$eq', "books"}}, {"likes", {'$gt', 100}])`

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
