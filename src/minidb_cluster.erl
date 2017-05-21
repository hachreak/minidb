%%%-------------------------------------------------------------------
%% @doc minidb public API
%% @end
%%%-------------------------------------------------------------------

-module(minidb_cluster).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  auto_join/0,
  join/1,
  leave/0,
  make/1,
  status/0
]).

%%====================================================================
%% API
%%====================================================================

status() ->
  % show the ring status.
  {ok, Ring} = riak_core_ring_manager:get_my_ring(),
  riak_core_ring:pretty_print(Ring, [legend]).

auto_join() ->
  case get_env(autojoin, false) of
    false -> ok;
    true ->
      case get_env(seeds, []) of
        [] -> {error, no_seeds};
        Seeds ->
          timer:apply_after(5000, minidb_cluster, make, [Seeds])
      end
  end.

make(Seeds) -> make(Seeds, Seeds).

join(SidNode) -> riak_core:join(SidNode).

leave() -> riak_core:leave().

%% Private functions

get_env(Config, Default) ->
  case application:get_env(minidb, cluster, []) of
    [] -> Default;
    ClusterConfig ->
      proplists:get_value(Config, ClusterConfig, Default)
  end.

make([], OriginalSeeds) ->
  ConnectedNodes = erlang:nodes(),
  Sub = ConnectedNodes -- OriginalSeeds,
  case erlang:length(Sub) < erlang:length(ConnectedNodes) of
    true ->ok;
    false -> {error, seeds_not_available}
  end;
make([Sid | Seeds], OriginalSeeds) ->
  case join(Sid) of
    {error, _} -> make(Seeds, OriginalSeeds);
    ok -> ok
  end.
