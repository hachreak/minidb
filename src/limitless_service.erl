%%%-------------------------------------------------------------------
%% @doc limitless_service public API
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service).

-export([ping/0]).

%%====================================================================
%% API
%%====================================================================

ping() ->
  DocIdx = riak_core_util:chash_key(
             {<<"ping">>, term_to_binary(os:timestamp())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, limitless_service),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(
    IndexNode, ping, limitless_service_vnode_master).
