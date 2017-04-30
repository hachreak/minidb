%%%-------------------------------------------------------------------
%% @doc minidb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(minidb_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

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
      minidb_vnode_master,
      {riak_core_vnode_master, start_link, [minidb_vnode]},
      permanent, 5000, worker, [riak_core_vnode_master]},

    CoverageFSMs = {
      minidb_coverage_fsm_sup,
      {minidb_coverage_fsm_sup, start_link, []},
      permanent, infinity, supervisor, [minidb_coverage_fsm_sup]},

    {ok, {{one_for_one, 5, 10}, [VMaster, CoverageFSMs]}}.

%%====================================================================
%% Internal functions
%%====================================================================
