%%%-------------------------------------------------------------------
%% @doc riak_core_demo coverage fsm supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(riak_core_demo_coverage_fsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_fsm/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_fsm(What) ->
  ReqId = make_reqid(),
  {ok, _} = supervisor:start_child(?MODULE, [ReqId, self(), What]),
  ReqId.

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
  CoverageFSM = {
    riak_core_demo_coverage_fsm,
    {riak_core_demo_coverage_fsm, start_link, []},
    temporary, 5000, worker, [riak_core_demo_coverage_fsm]},
  {ok, {{simple_one_for_one, 5, 10}, [CoverageFSM]}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_reqid() -> erlang:phash2(erlang:now()).
