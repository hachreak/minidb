%%%-------------------------------------------------------------------
%% @doc riak_core_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(riak_core_demo).

-export([ping/0]).

%%====================================================================
%% API
%%====================================================================

ping() ->
  DocIdx = riak_core_util:chash_key(
             {<<"ping">>, term_to_binary(os:timestamp())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_core_demo),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(
    IndexNode, ping, riak_core_demo_vnode_master).
