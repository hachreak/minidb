-module(minidb_vnode).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

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
  {ok, #state {partition=Partition, data=minidb_db_mem:init()}}.

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
  {noreply, State#state{data=minidb_db_mem:put(Key, Value, Data)}};
handle_command({patch, {Key, Patch}}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[patch] ~p -> ~p~n", [Key, Patch]),
  {noreply, State#state{data=minidb_db_mem:patch(Key, Patch, Data)}};
handle_command({get, Key}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[get] ~p~n", [Key]),
  {reply, minidb_db_mem:get(Key, Data), State};
handle_command({get, Key, Default}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[get] ~p default ~p~n", [Key, Default]),
  {reply, minidb_db_mem:get(Key, Data, Default), State};
handle_command({inc, Key, Queries}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[inc] ~p -> ~p~n", [Key, Queries]),
  {noreply, State#state{data=minidb_db_mem:inc(Key, Queries, Data)}};
handle_command({delete, Key}, _Sender, State=#state{data=Data}) ->
  error_logger:info_msg("[delete] ~p~n", [Key]),
  {noreply, State#state{data=minidb_db_mem:delete(Key, Data)}};
handle_command(Message, _Sender, State) ->
  lager:warning("unhandled_command ~p", [Message]),
  {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State=#state{partition=Partition, data=Data}) ->
  error_logger:info_msg("[handoff v2] partition ~p~n", [Partition]),
  AccFinal = minidb_db_mem:fold(fun(Key, Value, Acc) ->
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

handle_handoff_data(BinaryKV, State=#state{data=Data}) ->
  % Receive handoff data from the node that is switched off and decode it
  {reply, ok, State#state{data=minidb_db_mem:import(BinaryKV, Data)}}.

encode_handoff_item(Key, Value) ->
  % encode the data in a format that can be then transferred and then decoded
  % on other nodes.
  minidb_db_mem:export(Key, Value).

is_empty(State=#state{data=Data}) ->
  % if true, the handoff procedure is started!
  {minidb_db_mem:is_empty(Data), State}.

delete(State=#state{data=Data}) ->
  % called before the termination of the vnode and is used to clean the data
  {ok, State#state{data=minidb_db_mem:drop(Data)}}.

handle_coverage({keys, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{data=Data}) ->
  {reply, {RefId, minidb_db_mem:keys(Data)}, State};
handle_coverage({values, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{data=Data}) ->
  {reply, {RefId, minidb_db_mem:values(Data)}, State};
handle_coverage({drop, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{data=Data}) ->
  {reply, {RefId, minidb_db_mem:size(Data)}, State#state{data=#{}}};
handle_coverage({{find, Queries}, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{data=Data}) ->
  {reply, {RefId, minidb_db_mem:find(Queries, Data)}, State};
handle_coverage(Req, _KeySpaces, _Sender, State) ->
  error_logger:info_msg(
    "[handle coverage] Request ~p not implemented!", [Req]),
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% Private functions
