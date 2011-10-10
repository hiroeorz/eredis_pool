-module(eredis_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-import(eredis, [create_multibulk/1]).

-define(Setup, fun() -> application:start(eredis_pool)  end).
-define(Clearnup, fun(_) -> application:stop(eredis_pool)  end).


basic_test_() ->
    {inparallel,
  
     {setup, ?Setup, ?Clearnup,
      [

       { "get and set",
         fun() ->
                 ?assertMatch({ok, _}, eredis_pool:q(default, ["DEL", foo1])),
                 
                 ?assertEqual({ok, undefined}, 
                              eredis_pool:q(default, ["GET", foo1])),

                 ?assertEqual({ok, <<"OK">>}, 
                              eredis_pool:q(default, ["SET", foo1, bar])),

                 ?assertEqual({ok, <<"bar">>}, 
                              eredis_pool:q(default, ["GET", foo1]))
         end
       },

       { "delete test",
         fun() ->
                 ?assertMatch({ok, _}, eredis_pool:q(default, ["DEL", foo2])),
                 
                 ?assertEqual({ok, <<"OK">>}, 
                              eredis_pool:q(default, ["SET", foo2, bar])),

                 ?assertEqual({ok, <<"1">>}, 
                              eredis_pool:q(default, ["DEL", foo2])),

                 ?assertEqual({ok, undefined}, 
                              eredis_pool:q(default, ["GET", foo2]))
         end
       },

       { "mset and mget",
         fun() ->
                 Keys = lists:seq(1, 1000),
                 
                 ?assertMatch({ok, _}, eredis_pool:q(default, ["DEL" | Keys])),
                 
                 KeyValuePairs = [[K, K*2] || K <- Keys],
                 ExpectedResult = 
                     [list_to_binary(integer_to_list(K * 2)) || K <- Keys],
                 
                 ?assertEqual({ok, <<"OK">>}, 
                              eredis_pool:q(default, 
                                     ["MSET" | lists:flatten(KeyValuePairs)])),

                 ?assertEqual({ok, ExpectedResult}, 
                              eredis_pool:q(default, ["MGET" | Keys])),

                 ?assertMatch({ok, _}, eredis_pool:q(default, ["DEL" | Keys]))
         end
       },

       { "new pool create and delete",
         fun() ->
                 ?assertMatch({ok, _}, eredis_pool:create_pool(pool1, 10)),

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
        
