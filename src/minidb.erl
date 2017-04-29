%%%-------------------------------------------------------------------
%% @doc minidb public API
%% @end
%%%-------------------------------------------------------------------

-module(minidb).

-export([ping/0, put/2, get/1, keys/0, values/0, status/0]).

%%====================================================================
%% API
%%====================================================================

ping() ->
  riak_core_vnode_master:sync_command(
    get_node({<<"ping">>, term_to_binary(os:timestamp())}),
    ping, minidb_vnode_master).

put(Key, Value) ->
  riak_core_vnode_master:command(
    get_node({?MODULE, Key}), {put, {Key, Value}}, minidb_vnode_master).

get(Key) ->
  riak_core_vnode_master:sync_command(
    get_node({?MODULE, Key}), {get, Key}, minidb_vnode_master).

keys() ->
  ReqId = minidb_coverage_fsm_sup:start_fsm(keys),
  wait_result(ReqId).

values() ->
  ReqId = minidb_coverage_fsm_sup:start_fsm(values),
  wait_result(ReqId).

status() ->
  % show the ring status.
  {ok, Ring} = riak_core_ring_manager:get_my_ring(),
  riak_core_ring:pretty_print(Ring, [legend]).

%% Private functions

get_node(RingKey) ->
  DocIdx = riak_core_util:chash_key(RingKey),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, minidb),
  [{IndexNode, _Type}] = PrefList,
  IndexNode.

wait_result(ReqId) -> wait_result(ReqId, 5000).

wait_result(ReqId, Timeout) ->
  receive
    {ReqId, {ok, Result}} -> Result;
    {ReqId, {error, Reason}} -> {error, Reason}
  after
    Timeout -> {error, timeout}
  end.
