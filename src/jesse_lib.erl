%%%=============================================================================
%% Copyright 2012- Klarna AB
%% Copyright 2015- AUTHORS
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%% @end
%%%=============================================================================

-module(jesse_lib).

%% API
-export([ empty_if_not_found/1
        , get_schema_id/1
        , is_array/1
        , is_json_object/1
        , is_null/1
        , unwrap/1
        , to_json_path/1
        , get_value/3
        , get_path/2
        , get_path/3
        ]).

%% Includes
-include("jesse_schema_validator.hrl").

%%% API
%% @doc Returns an empty list if the given value is ?not_found.
-spec empty_if_not_found(Value :: any()) -> any().
empty_if_not_found(?not_found) -> [];
empty_if_not_found(Value)      -> Value.

%% @doc Returns value of "id" field from json object `Schema', assuming that
%% the given json object has such a field, otherwise an exception
%% will be thrown.
-spec get_schema_id(Schema :: jesse:json_term()) -> string().
get_schema_id(Schema) ->
  case get_value(?ID, Schema, ?not_found) of
    ?not_found -> throw({schema_invalid, Schema, missing_id_field});
    Id         -> erlang:binary_to_list(Id)
  end.

%% @doc Checks if the given value is json `array'.
%% This check is needed since objects in `jsx' are lists (proplists).
-spec is_array(Value :: any()) -> boolean().
is_array(Value) when is_list(Value) -> not is_json_object(Value);
is_array(_)                         -> false.

%% @doc A naive check if the given data is a json object.
%% Supports two main formats of json representation:
%% 1) mochijson2 format (`{struct, proplist()}')
%% 2) jiffy format (`{proplist()}')
%% 3) jsx format (`[{binary() | atom(), any()}]')
%% Returns `true' if the given data is an object, otherwise `false' is returned.
-spec is_json_object(Value :: any()) -> boolean().
is_json_object({struct, Value}) when is_list(Value) -> true;
is_json_object({Value}) when is_list(Value)         -> true;
%% handle `jsx' empty objects
is_json_object([{}])                                -> true;
%% very naive check. checks only the first element.
is_json_object([{Key, _Value} | _])
  when is_binary(Key) orelse is_atom(Key)
       andalso Key =/= struct                       -> true;
?IF_MAPS(is_json_object(Map) when erlang:is_map(Map) -> true;)
is_json_object(_)                                   -> false.

%% @doc Checks if the given value is json `null'.
-spec is_null(Value :: any()) -> boolean().
is_null(null)   -> true;
is_null(_Value) -> false.

%% @doc Unwrap data (remove mochijson2 and jiffy specific constructions,
%% and also handle `jsx' empty objects)
-spec unwrap(kvc:kvc_obj()) -> kvc:kvc_obj().
unwrap({struct, L}) -> L;
unwrap({L}) -> L;
unwrap({}) -> [];
unwrap([]) -> [];
unwrap([{}]) -> [];
?IF_MAPS(unwrap(Map) when erlang:is_map(Map) -> maps:to_list(Map);)
unwrap(L) -> L.

%% @doc Parse a JSON Pointer
-spec to_json_path(JSONPointer :: string() | binary()) -> [binary()].
to_json_path(JSONPointer) ->
  lists:map( fun parse_json_pointer_token/1
           , re:split(JSONPointer, "/", [{return, list}])
           ).

-spec get_value(kvc:kvc_key(), kvc:kvc_obj(), term()) -> term().
get_value(Key, Schema, Default) ->
  kvc:value(Key, Schema, Default).

-spec get_path(kvc:kvc_key() | [kvc:kvc_key()], kvc:kvc_obj()) -> term() | [].
get_path(Key, Schema) ->
  get_path(Key, Schema, []).

-spec get_path(kvc:kvc_key() | [kvc:kvc_key()], kvc:kvc_obj(), term()) -> term() | [].
get_path(Key, Schema, Default) ->
  kvc:path(Key, Schema, Default).

%% Internal API

-spec parse_json_pointer_token(Token :: string()) -> binary().
parse_json_pointer_token(Token) ->
    DecodedToken = unicode:characters_to_binary(http_uri:decode(Token)),
    lists:foldl( fun({From, To}, T) ->
                         binary:replace(T, From, To)
                 end
               , DecodedToken
               , [ {<<"~0">>, <<"~">>}
                 , {<<"~1">>, <<"/">>}
                 ]
               ).
