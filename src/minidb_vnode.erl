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

-record(state, {partition, backend, ctx}).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  {Backend, Config} = get_backend(),
  {ok, #state{partition=Partition, backend=Backend, ctx=Backend:init(Config)}}.

handle_overload_command(_, _, _) ->
  lager:warning("[VNODE] Overload command!~n").

handle_overload_info(_, _) ->
  lager:warning("[VNODE] Overload info!~n").

%% Sample command: respond to a ping
handle_command(ping, Sender, State) ->
  error_logger:info_msg("[ping received] from ~p~n", [Sender]),
  {reply, {pong, State#state.partition}, State};

handle_command({put, {Key, Value}}, _Sender,
               State=#state{backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[put] ~p -> ~p~n", [Key, Value]),
  {noreply, State#state{ctx=Backend:put(Key, Value, Ctx)}};

handle_command({patch, {Key, Patch}}, _Sender,
               State=#state{backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[patch] ~p -> ~p~n", [Key, Patch]),
  {noreply, State#state{ctx=Backend:patch(Key, Patch, Ctx)}};

handle_command({get, Key}, _Sender,
               State=#state{backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[get] ~p~n", [Key]),
  {reply, Backend:get(Key, Ctx), State};

handle_command({get, Key, Default}, _Sender,
               State=#state{backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[get] ~p default ~p~n", [Key, Default]),
  {reply, Backend:get(Key, Ctx, Default), State};

handle_command({inc, Key, Queries}, _Sender,
               State=#state{backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[inc] ~p -> ~p~n", [Key, Queries]),
  {noreply, State#state{ctx=Backend:inc(Key, Queries, Ctx)}};

handle_command({delete, Key}, _Sender,
               State=#state{backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[delete] ~p~n", [Key]),
  {noreply, State#state{ctx=Backend:delete(Key, Ctx)}};

handle_command(Message, _Sender, State) ->
  lager:warning("unhandled_command ~p", [Message]),
  {noreply, State}.

handle_handoff_command(
    ?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
    State=#state{partition=Partition, backend=Backend, ctx=Ctx}) ->
  error_logger:info_msg("[handoff v2] partition ~p~n", [Partition]),
  AccFinal = Backend:fold(fun(Key, Value, Acc) ->
      FoldFun(Key, Value, Acc)
    end, Acc0, Ctx),
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

handle_handoff_data(BinaryKV, State=#state{backend=Backend, ctx=Ctx}) ->
  % Receive handoff data from the node that is switched off and decode it
  {Key, Value} = erlang:binary_to_term(BinaryKV),
  {reply, ok, State#state{ctx=Backend:put(Key, Value, Ctx)}}.

encode_handoff_item(Key, Value) ->
  % encode the data in a format that can be then transferred and then decoded
  % on other nodes.
  erlang:term_to_binary({Key, Value}).

is_empty(State=#state{backend=Backend, ctx=Ctx}) ->
  % if true, the handoff procedure is started!
  {Backend:is_empty(Ctx), State}.

delete(State=#state{backend=Backend, ctx=Ctx}) ->
  % called before the termination of the vnode and is used to clean the data
  {ok, State#state{ctx=Backend:drop(Ctx)}}.

handle_coverage({keys, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{backend=Backend, ctx=Ctx}) ->
  {reply, {RefId, Backend:keys(Ctx)}, State};

handle_coverage({values, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{backend=Backend, ctx=Ctx}) ->
  {reply, {RefId, Backend:values(Ctx)}, State};

handle_coverage({drop, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{backend=Backend, ctx=Ctx}) ->
  {reply, {RefId, Backend:size(Ctx)}, State#state{ctx=Backend:drop(Ctx)}};

handle_coverage({{find, Queries}, _, _}, _KeySpaces, {_, RefId, _},
                State=#state{backend=Backend, ctx=Ctx}) ->
  {reply, {RefId, Backend:find(Queries, Ctx)}, State};

handle_coverage(Req, _KeySpaces, _Sender, State) ->
  error_logger:info_msg(
    "[handle coverage] Request ~p not implemented!", [Req]),
  {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% Private functions

get_backend() ->
  application:get_env(minidb, backend, {minidb_db_mem, #{}}).
