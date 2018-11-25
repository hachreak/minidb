%%%-------------------------------------------------------------------
%% @doc minidb db ets backend
%% @end
%%%-------------------------------------------------------------------

-module(minidb_db_ets).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  all/1,
  delete/2,
  drop/1,
  find/2,
  fold/3,
  get/2,
  get/3,
  inc/3,
  init/1,
  is_empty/1,
  keys/1,
  patch/3,
  put/3,
  size/1,
  values/1
]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%====================================================================
%% API
%%====================================================================

init(Config) ->
  Index = create(Config),
  #{index => Index, cfg => Config}.

put(Key, Value, #{index := Index}=Ctx) ->
  ets:insert(Index, {Key, Value}),
  Ctx.

patch(Key, Patch, Ctx) ->
  case get(Key, Ctx) of
    none ->
      put(Key, Patch, Ctx);
    Value ->
      FinalValue = maps:merge(Value, Patch),
      put(Key, FinalValue, Ctx)
  end,
  Ctx.

get(Key, Ctx) -> get(Key, Ctx, none).

get(Key, #{index := Index}, Default) ->
  case ets:lookup(Index, Key) of
    [] -> Default;
    [{_, Value}] -> Value
  end.

find(Queries, #{index := Index}) ->
  lists:sort(qlc:eval(qlc_query(build(Queries), Index))).

% TODO
inc(_Key, _Queries, _Ctx) ->
  throw(not_implemented).

delete(Key, #{index := Index}=Ctx) ->
  ets:delete(Index, Key),
  Ctx.

fold(Fun, Init, #{index := Index}) ->
  ets:foldl(fun({Key, Value}, Acc) ->
      Fun(Key, Value, Acc)
    end, Init, Index).

is_empty(#{index := Index}) ->
  ets:first(Index) =:= '$end_of_table'.

drop(#{index := Index, cfg := Config}) ->
  ets:delete(Index),
  init(Config).

all(#{index := Index}) ->
  ets:select(Index, ets:fun2ms(fun(_X) -> true end)).

keys(#{index := Index}) ->
  ets:select(Index, ets:fun2ms(fun({K, _}) -> K end)).

values(#{index := Index}) ->
  ets:select(Index, ets:fun2ms(fun({_, V}) -> V end)).

size(#{index := Index}) ->
  ets:select_count(Index, ets:fun2ms(fun(_X) -> true end)).

%% Private functions

-define(BASE_Q, "is_map(V)").
-define(SEP_Q, " andalso ").
-define(Q, "maps:get({{k}}, V, none) {{op}} {{i}}").
-define(OPs, #{
  '$eq'  => '=:=',
  '$gt'  => '>',
  '$gte' => '>=',
  '$lt'  => '<',
  '$lte' => '=<',
  '$ne'  => 'not'
}).

build(Queries) ->
  Vars = vars(Queries),
  compile(query(join_operators(Vars)), bindings(Vars)).

enumerate(Queries) ->
  lists:zip(lists:seq(0, length(Queries) - 1), Queries).

vars(Queries) ->
  lists:map(fun({Index, {QueryKey, {Operator, QueryValue}}}) ->
      Ki = sf:format("K{{index}}", [{index, Index}], [atom]),
      Ii = sf:format("I{{index}}", [{index, Index}], [atom]),
      [{Ki, QueryKey}, {Ii, QueryValue}, {op, Operator}]
    end, enumerate(Queries)).

join_operators(Vars) ->
  lists:map(fun([{K, _}, {I, _}, {_, Op}]) ->
      Eop = maps:get(Op, ?OPs),
      sf:format(?Q, [{k, K}, {op, Eop}, {i, I}], [string])
    end, Vars).

query(Operators) ->
  string:join([?BASE_Q | Operators], ?SEP_Q) ++ ".".

bindings(Vars) ->
  lists:merge(lists:map(fun([K, I, _O]) -> [K, I] end, Vars)).

compile(Query, Bindings) ->
  {ok, Scan, _} = erl_scan:string(Query),
  {ok, Parse} = erl_parse:parse_exprs(Scan),
  fun(V) ->
    fun() ->
      {value, Res, _} = erl_eval:exprs(Parse, [{'V', V} | Bindings]),
      Res
    end
  end.

qlc_query(Fun, Index) ->
  qlc:q([V || {_,V} <- ets:table(Index), begin Fun(V) end ()]).

create(Config) ->
  Name = maps:get(name, Config, ?MODULE),
  ets:new(Name, [
    set, private, {keypos, 1},
    {read_concurrency, true},
    {write_concurrency, false}
  ]).
