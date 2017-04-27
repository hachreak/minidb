%%%-------------------------------------------------------------------
%% @doc riak_core_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(riak_core_demo).

-export([ping/0, put/2, get/1]).

%%====================================================================
%% API
%%====================================================================

ping() ->
  riak_core_vnode_master:sync_command(
    get_node({<<"ping">>, term_to_binary(os:timestamp())}),
    ping, riak_core_demo_vnode_master).

put(Key, Value) ->
  riak_core_vnode_master:command(
    get_node({?MODULE, Key}), {put, {Key, Value}}, riak_core_demo_vnode_master).

get(Key) ->
  riak_core_vnode_master:sync_command(
    get_node({?MODULE, Key}), {get, Key}, riak_core_demo_vnode_master).

%% Private functions

get_node(RingKey) ->
  DocIdx = riak_core_util:chash_key(RingKey),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_core_demo),
  [{IndexNode, _Type}] = PrefList,
  IndexNode.
