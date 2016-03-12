%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc EUnit tests for jesse_json_path.
-module(jesse_json_path_tests).
-include_lib("eunit/include/eunit.hrl").

path_aggregate_test() ->
    ?assertEqual(
       [taco, taco, grande],
       jesse_lib:get_path(foo, [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       3,
       jesse_lib:get_path(<<"foo.@count">>, [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       6,
       jesse_lib:get_path('foo.@sum', [{foo, [1, 2, 3]}])),
    ?assertEqual(
       2.0,
       jesse_lib:get_path("foo.@avg", [{foo, [1, 2, 3]}])),
    ?assertEqual(
       1,
       jesse_lib:get_path([foo, '@min'], [{foo, [1, 2, 3]}])),
    ?assertEqual(
       3,
       jesse_lib:get_path("foo.@max", [{foo, [1, 2, 3]}])),
    ?assertEqual(
       [taco, taco, grande],
       jesse_lib:get_path(<<"foo.@unionOfObjects">>,
                            [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       [taco, taco, grande],
       jesse_lib:get_path(<<"foo.@unionOfArrays">>,
                            [{foo, [[taco], [taco], [grande]]}])),
    ?assertEqual(
       lists:sort([taco, grande]),
       lists:sort(jesse_lib:get_path(<<"foo.@distinctUnionOfObjects">>,
                                       [{foo, [taco, taco, grande]}]))),
    ?assertEqual(
       lists:sort([taco, grande]),
       lists:sort(jesse_lib:get_path(<<"foo.@distinctUnionOfArrays">>,
                                       [{foo, [[taco], [taco], [grande]]}]))),
    ok.

value_aggregate_test() ->
    ?assertEqual(
       6,
       jesse_lib:get_value('@sum', [1, 2, 3], [])),
    ?assertEqual(
       6,
       jesse_lib:get_value("@sum", [1, 2, 3], [])),
    ?assertEqual(
       6,
       jesse_lib:get_value(<<"@sum">>, [1, 2, 3], [])),
    ?assertEqual(
       2.0,
       jesse_lib:get_value(<<"@avg">>, [1, 2, 3], [])),
    ?assertEqual(
       [],
       jesse_lib:get_value(<<"@avg">>, [], [])),
    ok.

path_edge_test() ->
    ?assertEqual(
       [bar],
       jesse_lib:get_path(foo, [[{foo, bar}], [{bar, baz}]])),
    ?assertEqual(
       [bar],
       jesse_lib:get_path(foo, [[{foo, bar}], [{bar, baz}]])),
    ok.

value_edge_test() ->
    ?assertEqual(
       [],
       jesse_lib:get_value(foo, [{1, 2}], [])),
    ?assertEqual(
       [],
       jesse_lib:get_value(<<255>>, [{foo, ok}], [])),
    ?assertEqual(
       [],
       jesse_lib:get_value([256], [{foo, ok}], [])),
    ok.

path_plist_test() ->
    lists:foreach(
      fun (F) ->
              ?assertEqual(
                 baz,
                 jesse_lib:get_path('foo.bar', F([{foo, [{bar, baz}]}]))),
              ?assertEqual(
                 [],
                 jesse_lib:get_path('foo.bar', F([{foo, [{baz, baz}]}]))),
              ?assertEqual(
                 [],
                 jesse_lib:get_path('foo.bar', F([{not_foo, ok}]))),
              ?assertEqual(
                 [],
                 jesse_lib:get_path('foo.bar', F([])))
      end,
      [fun gb_trees:from_orddict/1, fun dict:from_list/1]),
    ?assertEqual(
       wibble,
       jesse_lib:get_path('foo.bar.baz', [{foo, [{bar, [{baz, wibble}]}]}])),
    ?assertEqual(
       [],
       jesse_lib:get_path('foo.bar.baz.invalid_proplist',
                            [{foo, [{bar, [{baz, wibble}]}]}])),
    ?assertEqual(
       [],
       jesse_lib:get_path('foo.bar.baz', [{foo, [{bar, [{bar, wibble}]}]}])),
    ?assertEqual(
       <<"wibble">>,
       jesse_lib:get_path('foo.bar.baz',
                            {struct,
                             [{<<"foo">>,
                               {struct,
                                [{<<"bar">>,
                                  {struct, [{<<"baz">>, <<"wibble">>}]}}]}}]})),
    ?assertEqual(
       <<"wibble">>,
       jesse_lib:get_path('foo.bar.baz',
                            {[{<<"foo">>,
                               {[{<<"bar">>,
                                  {[{<<"baz">>, <<"wibble">>}]}}]}}]})),
    ?assertEqual(
       "wibble",
       jesse_lib:get_path('foo.bar.baz',
                            {struct,
                             [{"foo",
                               {struct,
                                [{"bar",
                                  {struct, [{"baz", "wibble"}]}}]}}]})),
    ?assertEqual(
       ok,
       jesse_lib:get_value("foo", [{foo, ok}], [])),
    ?assertEqual(
       ok,
       jesse_lib:get_value("foo", [{<<"foo">>, ok}], [])),
    ?assertEqual(
       ok,
       jesse_lib:get_value("foo", {}, ok)),
    ok.

path_default_test() ->
    %% This shouldn't hit the default path at all
    ?assertEqual(
       1,
       jesse_lib:get_path([derp], [{derp, 1}])),
    ?assertEqual(
       1,
       jesse_lib:get_path([derp], [{derp, 1}], unused)),
    %% The standard default is []
    ?assertEqual(
       [],
       jesse_lib:get_path([foo], [{derp, 1}])),
    ?assertEqual(
       default,
       jesse_lib:get_path([foo], [{derp, 1}], default)),
    %% The default is *not* recursive
    ?assertEqual(
       [{bar, baz}],
       jesse_lib:get_path([foo, bar], [], [{bar, baz}])),
    ?assertEqual(
       [{foo, bar}],
       jesse_lib:get_path([foo, bar], [], [{foo, bar}])).

jsx_object_test() ->
    ?assertEqual(
       not_found,
       jesse_lib:get_value(<<"foo">>, [{}], not_found)).

-ifndef(erlang_deprecated_types).
map_object_test_() ->
    [?_assertEqual(
        not_found,
        jesse_lib:get_path(
          "foo.bar", #{<<"foo">> => #{<<"baz">> => val}}, not_found)),
     ?_assertEqual(
        val,
        jesse_lib:get_path(
          "foo.bar", #{<<"foo">> => #{<<"bar">> => val}}, not_found)),
     ?_assertEqual(
        #{a => b},
        jesse_lib:get_path(
          [foo, bar], #{foo => #{bar => #{a => b}}}, not_found))].
-endif.
