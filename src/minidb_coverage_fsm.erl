%%%-------------------------------------------------------------------
%% @doc minidb coverage fsm
%% @end
%%%-------------------------------------------------------------------

-module(minidb_coverage_fsm).

-behaviour(riak_core_coverage_fsm).

-define(TIMEOUT, 5000).
-define(N_VAL, 1).
-define(VNODE_COVERAGE, 1).

%% Application callbacks
-export([
  finish/2,
  init/2,
  process_results/2,
  start_link/3
]).

start_link(ReqId, From, What) ->
  error_logger:info_msg("[FSM] start coverage!~n"),
  riak_core_coverage_fsm:start_link(
    ?MODULE, {pid, ReqId, From}, [ReqId, From, What, ?TIMEOUT]).

init(_From, [ReqId, From, What, Timeout]=Args) ->
  error_logger:info_msg("[CoverateFsm] Init~n"),
  {
    {What, ReqId, From},
    % we need to execute the command on all the running nodes,
    % not on all the nodes in the cluster
    allup,
    % two arguments that specify how to manage the replication in our cluster.
    ?N_VAL,
    ?VNODE_COVERAGE,
    % service used to identify the running nodes
    minidb,
    % and the module that implements the vnode
    minidb_vnode_master,
    Timeout,
    riak_core_coverage_plan,
    % FSM state
    #{from => From, reqid => ReqId, args => Args, result => []}
  }.

process_results({_Partition, Value}, #{result := Result}=State) ->
  % called for every response received from the vnode.
  error_logger:info_msg(
    "[CoverateFsm] Process result, ~nValue: ~p~n", [Value]),
  {done, State#{result => [Value | Result]}}.

finish(clean, #{from := From, reqid := ReqId, result := Result}=State) ->
  error_logger:info_msg("[CoverateFsm] Finish~ninspect state:~n~p~n", [State]),
  From ! {ReqId, {ok, lists:flatten(Result)}},
  {stop, normal, State};
finish({error, Reason}, #{from :=From, reqid := ReqId}=State) ->
  error_logger:info_msg("[CoverateFsm] Error: ~p~n", [Reason]),
  From ! {ReqId, {error, Reason}},
  {stop, normal, State}.
