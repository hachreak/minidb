%%%-------------------------------------------------------------------
%% @doc riak_core_demo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(riak_core_demo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    VMaster = {
      riak_core_demo_vnode_master,
      {riak_core_vnode_master, start_link, [riak_core_demo_vnode]},
      permanent, 5000, worker, [riak_core_vnode_master]},
    {ok, {{one_for_one, 5, 10}, [VMaster]}}.

%%====================================================================
%% Internal functions
%%====================================================================
