-module(eredis_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-import(eredis, [create_multibulk/1]).

-define(Setup, fun() -> application:start(eredis_pool)  end).
-define(Clearnup, fun(_) -> application:stop(eredis_pool)  end).
-define(DEFAULT, dbsrv).

transaction_test_() ->
    {inparallel,
     {setup, ?Setup, ?Clearnup,
      [

       { "transaction",
         fun() ->
                 eredis_pool:q(?DEFAULT, ["DEL", queue1, bar]),
                 eredis_pool:q(?DEFAULT, ["DEL", queue2, bar]),

                 {ok, _} =
                     eredis_pool:q(?DEFAULT, ["RPUSH", queue1, bar]),

                 Fun =
                     fun(C) ->
                             {ok, <<"QUEUED">>} = eredis:q(C, ["LREM",
                                                               queue1, 1,
                                                               bar]),

                             {ok, <<"QUEUED">>} = eredis:q(C, ["RPUSH",
                                                               queue2,
                                                               bar])
                     end,

                 {ok, [<<"1">>, <<"1">>]} =
                     eredis_pool:transaction(?DEFAULT, Fun),

                 ?assertEqual({ok, <<"0">>},
                              eredis_pool:q(?DEFAULT, ["LLEN", queue1])),
                 ?assertEqual({ok, <<"1">>},
                              eredis_pool:q(?DEFAULT, ["LLEN", queue2]))
         end
       },

       { "rollback",
         fun() ->
                 eredis_pool:q(?DEFAULT, ["DEL", queue3, bar]),
                 eredis_pool:q(?DEFAULT, ["DEL", queue4, bar]),

                 {ok, _} =
                     eredis_pool:q(?DEFAULT, ["RPUSH", queue3, bar]),

                 Fun =
                     fun(C) ->
                             {ok, <<"QUEUED">>} = eredis:q(C, ["LREM",
                                                               queue3, 1,
                                                               bar]),
                             throw(normal)
                     end,

		 {throw, normal} = eredis_pool:transaction(?DEFAULT, Fun),

                 ?assertEqual({ok, <<"1">>},
                              eredis_pool:q(?DEFAULT, ["LLEN", queue3])),
                 ?assertEqual({ok, <<"0">>},
                              eredis_pool:q(?DEFAULT, ["LLEN", queue4]))
         end
       }

      ]
     }
    }.

basic_test_() ->
    {inparallel,

     {setup, ?Setup, ?Clearnup,
      [

       { "get and set",
         fun() ->
                 ?assertMatch({ok, _}, eredis_pool:q(?DEFAULT, ["DEL", foo1])),

                 ?assertEqual({ok, undefined},
                              eredis_pool:q(?DEFAULT, ["GET", foo1])),

                 ?assertEqual({ok, <<"OK">>},
                              eredis_pool:q(?DEFAULT, ["SET", foo1, bar])),

                 ?assertEqual({ok, <<"bar">>},
                              eredis_pool:q(?DEFAULT, ["GET", foo1]))
         end
       },

       { "delete test",
         fun() ->
                 ?assertMatch({ok, _}, eredis_pool:q(?DEFAULT, ["DEL", foo2])),

                 ?assertEqual({ok, <<"OK">>},
                              eredis_pool:q(?DEFAULT, ["SET", foo2, bar])),

                 ?assertEqual({ok, <<"1">>},
                              eredis_pool:q(?DEFAULT, ["DEL", foo2])),

                 ?assertEqual({ok, undefined},
                              eredis_pool:q(?DEFAULT, ["GET", foo2]))
         end
       },

       { "mset and mget",
         fun() ->
                 Keys = lists:seq(1, 1000),

                 ?assertMatch({ok, _}, eredis_pool:q(?DEFAULT, ["DEL" | Keys])),

                 KeyValuePairs = [[K, K*2] || K <- Keys],
                 ExpectedResult =
                     [list_to_binary(integer_to_list(K * 2)) || K <- Keys],

                 ?assertEqual({ok, <<"OK">>},
                              eredis_pool:q(?DEFAULT,
                                            ["MSET" | lists:flatten(KeyValuePairs)])),

                 ?assertEqual({ok, ExpectedResult},
                              eredis_pool:q(?DEFAULT, ["MGET" | Keys])),

                 ?assertMatch({ok, _}, eredis_pool:q(?DEFAULT, ["DEL" | Keys]))
         end
       },

       { "new pool create and delete",
         fun() ->
                 ?assertMatch({ok, _},
                              eredis_pool:create_pool(pool1, 10)),

                 ?assertMatch({ok, _}, eredis_pool:q(pool1, ["DEL", foo1])),

                 ?assertEqual({ok, undefined},
                              eredis_pool:q(pool1, ["GET", foo1])),

                 ?assertEqual({ok, <<"OK">>},
                              eredis_pool:q(pool1, ["SET", foo1, bar])),

                 ?assertEqual({ok, <<"bar">>},
                              eredis_pool:q(pool1, ["GET", foo1])),

                 ?assertEqual(ok, eredis_pool:delete_pool(pool1))
         end
       }

      ]
     }
    }.
