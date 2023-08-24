-module(proper_json).

-export([json/0, json/1]).

-include_lib("proper/include/proper.hrl").


json() ->
    ?SIZED(Size, json(Size)).


json(0) ->
    j_literal();
json(Size) ->
    ?LAZY(proper_types:frequency(
            [{60, j_literal()},
             {20, j_array(Size)},
             {20, j_object(Size)}])).


j_object(0) ->
    #{};
j_object(Size) ->
    ?LET(KV,
         proper_types:list({j_string(), json(Size div 4)}),
         maps:from_list(KV)).


j_array(0) ->
    [];
j_array(Size) ->
    proper_types:list(json(Size div 4)).


j_string() ->
    %% proper_unicode:utf8().
    ?LET(Str,
         proper_types:non_empty(chars()),
         list_to_binary(Str)).


j_literal() ->
    proper_types:oneof(
      [j_string(),
       proper_types:integer(),
       proper_types:float(),
       proper_types:boolean(),
       null]).


chars() ->
    proper_types:list(
      proper_types:oneof(lists:seq($a, $z) ++
                         lists:seq($A, $Z) ++
                         lists:seq($0, $9))).
