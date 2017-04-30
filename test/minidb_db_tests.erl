%%%-------------------------------------------------------------------
%% @doc minidb db memory backend tests
%% @end
%%%-------------------------------------------------------------------

-module(minidb_db_tests).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-include_lib("eunit/include/eunit.hrl").


find_test() ->
  Data = #{
    a => #{
      title => "hello",
      section => "erlang",
      likes => 4,
      created => {1493,569118,339095}
    },
    b => #{
      title => "world",
      section => "welcome",
      likes => 7,
      created => {1493,569118,338090}
    },
    c => #{
      title => "homepage",
      section => "erlang",
      likes => 21,
      created => {1493,569118,340099}
    },
    d => #{
      title => "welcome",
      section => "welcome",
      likes => 7,
      created => {1493,569118,337090}
    }
  },
  ExpectedKeys = [maps:get(a, Data), maps:get(c, Data)],
  Query = [
    {section, {'$eq', "erlang"}},
    {created, {'$gt', {1493,569118,339000}}}
  ],
  ?assertEqual(ExpectedKeys, minidb_db:find(Query, Data)).
