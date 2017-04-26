%%%-------------------------------------------------------------------
%% @doc riak_core_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(riak_core_demo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  case riak_core_demo_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, riak_core_demo_vnode}]),
      ok = riak_core_node_watcher:service_up(riak_core_demo, self()),

      {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
