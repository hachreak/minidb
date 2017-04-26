%%%-------------------------------------------------------------------
%% @doc limitless_service public API
%% @end
%%%-------------------------------------------------------------------

-module(limitless_service_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  case limitless_service_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, limitless_service_vnode}]),
      ok = riak_core_node_watcher:service_up(limitless_service, self()),

      {ok, Pid};
    {error, Reason} -> {error, Reason}
  end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
