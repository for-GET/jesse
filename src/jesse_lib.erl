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
        , is_array/1
        , is_json_object/1
        , is_null/1
        , re_run/2
        , re_options/0
        ]).

%% Includes
-include("jesse_schema_validator.hrl").

%%% API
%% @doc Returns an empty list if the given value is ?not_found.
-spec empty_if_not_found(Value :: any()) -> any().
empty_if_not_found(?not_found) ->
  [];
empty_if_not_found(Value) ->
  Value.

%% @doc Checks if the given value is json `array'.
%% This check is needed since objects in `jsx' are lists (proplists).
-spec is_array(Value :: any()) -> boolean().
is_array(Value)
  when is_list(Value) ->
  not is_json_object(Value);
is_array(_) ->
  false.

%% @doc A naive check if the given data is a json object.
%% Supports two main formats of json representation:
%% 1) mochijson2 format (`{struct, proplist()}')
%% 2) jiffy format (`{proplist()}')
%% 3) jsx format (`[{binary() | atom(), any()}]')
%% Returns `true' if the given data is an object, otherwise `false' is returned.
-spec is_json_object(Value :: any()) -> boolean().
?IF_MAPS(
is_json_object(Map)
  when erlang:is_map(Map) ->
  true;
)
is_json_object({struct, Value})
  when is_list(Value) ->
  true;
is_json_object({Value})
  when is_list(Value) ->
  true;
%% handle `jsx' empty objects
is_json_object([{}]) ->
  true;
%% very naive check. checks only the first element.
is_json_object([{Key, _Value} | _])
  when is_binary(Key) orelse is_atom(Key)
       andalso Key =/= struct ->
  true;
is_json_object(_) ->
  false.

%% @doc Checks if the given value is json `null'.
-spec is_null(Value :: any()) -> boolean().
is_null(null) ->
  true;
is_null(_Value) ->
  false.

%% @doc Run the RE against the subject using the `re_options' from the jesse
%% application environment. `{capture, none}' is always used.
-spec re_run( Subject :: iodata() | unicode:charlist()
            , RE :: iodata() | unicode:charlist()
            ) -> match
               | nomatch.
re_run(Subject, RE) ->
  re:run(Subject, RE, [{capture, none} | re_options()]).

%% @doc Returns the base re options from jesse environment which will be used
%% when running client-provided patterns. By default, that is `[unicode, ucp]'
%% for the fullest compatibility matching unicode code points beyond ISO Latin-1
%% in character classes like `\w', `\s', and `\d'. Use only `[unicode]' instead
%% (without `ucp`) for better performance at the expense of full non-ISO Latin-1
%% compatibility in character classes. See also notes on the `ucp' option at
%% [https://www.erlang.org/doc/man/re.html#compile-2 re:compile/2].
-spec re_options() -> list().
re_options() ->
  application:get_env(jesse, re_options, [unicode, ucp]).
