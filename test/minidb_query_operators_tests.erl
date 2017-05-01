%%%-------------------------------------------------------------------
%% @doc minidb db memory backend tests
%% @end
%%%-------------------------------------------------------------------

-module(minidb_query_operators_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").


gt_test() ->
  ?assertEqual(
     true,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1490,569118,339095})),
  ?assertEqual(
     true,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1493,569110,339095})),
  ?assertEqual(
     true,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1493,569118,339090})),
  ?assertEqual(
     false,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1493,569118,339095})),
  ?assertEqual(
     false,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1495,569118,339095})),
  ?assertEqual(
     false,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1493,569119,339095})),
  ?assertEqual(
     false,
     minidb_query_operators:'$gt'({1493,569118,339095}, {1493,569118,339098})),

  ?assertEqual(false, minidb_query_operators:'$gt'(1493, 339098)),
  ?assertEqual(false, minidb_query_operators:'$gt'(1493, 1493)),
  ?assertEqual(true, minidb_query_operators:'$gt'(1493, 143)),

  ok.
