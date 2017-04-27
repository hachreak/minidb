-module(riak_core_demo_vnode).
-behaviour(riak_core_vnode).

-export([
  delete/1,
  encode_handoff_item/2,
  handle_command/3,
  handle_coverage/4,
  handle_exit/3,
  handle_handoff_command/3,
  handle_handoff_data/2,
  handle_overload_command/3,
  handle_overload_info/2,
  handoff_cancelled/1,
  handoff_finished/2,
  handoff_starting/2,
  init/1,
  is_empty/1,
  start_vnode/1,
  terminate/2
]).

-ignore_xref([
  start_vnode/1
]).

-record(state, {partition, data}).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  {ok, #state { partition=Partition, data=#{}}}.

handle_overload_command(_, _, _) ->
  lager:warning("[VNODE] Overload command!~n").

handle_overload_info(_, _) ->
  lager:warning("[VNODE] Overload info!~n").

%% Sample command: respond to a ping
handle_command(ping, Sender, State) ->
  error_logger:info_msg("[ping received] from ~p~n", [Sender]),
  {reply, {pong, State#state.partition}, State};
handle_command({put, {Key, Value}}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[put] ~p -> ~p~n", [Key, Value]),
  {noreply, State#state{data=Data#{Key => Value}}};
handle_command({get, Key}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[get] ~p~n", [Key]),
  {reply, maps:get(Key, Data), State};
handle_command(Message, _Sender, State) ->
  lager:warning("unhandled_command ~p", [Message]),
  {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
  {noreply, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(_Data, State) ->
  {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

is_empty(State) ->
  {true, State}.

delete(State) ->
  {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

