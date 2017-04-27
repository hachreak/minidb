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

-include_lib("riak_core/include/riak_core_vnode.hrl").

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

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State=#state{partition=Partition, data=Data}) ->
  error_logger:info_msg("[handoff v2] partition ~p~n", [Partition]),
  AccFinal = maps:fold(fun(Key, Value, Acc) ->
      FoldFun(Key, Value, Acc)
    end, Acc0, Data),
  {reply, AccFinal, State};
handle_handoff_command(Message, Sender, State) ->
  error_logger:info_msg("[handoff generic request]"),
  handle_command(Message, Sender, State),
  {noreply, State}.

handoff_starting(TargetNode, State) ->
  error_logger:info_msg("[Handoff] started from ~p~n", [TargetNode]),
  % return false if you want to postpone the procedure
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(TargetNode, State) ->
  error_logger:info_msg("[Handoff] finish from ~p~n", [TargetNode]),
  {ok, State}.

handle_handoff_data(BinaryData, State=#state{data=Data}) ->
  % Receive handoff data from the node that is switched off and decode it
  {Key, Value} = erlang:binary_to_term(BinaryData),
  {reply, ok, State#state{data=Data#{Key => Value}}}.

encode_handoff_item(Key, Value) ->
  % encode the data in a format that can be then transferred and then decoded
  % on other nodes.
  erlang:term_to_binary({Key, Value}).

is_empty(State=#state{data=Data}) ->
  % if true, the handoff procedure is started!
  {maps:size(Data) =:= 0, State}.

delete(State) ->
  % called before the termination of the vnode and is used to clean the data
  {ok, State#state{data=#{}}}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

