%%%-------------------------------------------------------------------
%% @doc minidb public API
%% @end
%%%-------------------------------------------------------------------

-module(minidb_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  case minidb_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, minidb_vnode}]),
      ok = riak_core_node_watcher:service_up(minidb, self()),
      {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
