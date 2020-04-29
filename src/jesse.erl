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
%% @doc JESSE (JSon Schema Erlang)
%%
%% This is an interface module which provides an access to the main
%% functionality of jesse, such as 1) updating of the schema definitions cache;
%% 2) validation json data against a schema.
%% @end
%%%=============================================================================

-module(jesse).

%% API
-export([ main/1
        , add_schema/2
        , add_schema/3
        , del_schema/1
        , load_schemas/2
        , load_schemas/3
        , validate/2
        , validate/3
        , validate_definition/3
        , validate_definition/4
        , validate_local_ref/3
        , validate_local_ref/4
        , validate_with_schema/2
        , validate_with_schema/3
        ]).

-export_type([ allowed_errors/0
             , error_handler/0
             , error_list/0
             , external_validator/0
             , json_term/0
             , schema/0
             , schema_id/0
             , schema_ref/0
             , schema_ver/0
             , schema_loader_fun/0
             , option/0
             , options/0
             ]).

%% Includes
-include("jesse_schema_validator.hrl").

%% Internal datastructures
-type allowed_errors() :: non_neg_integer()
                        | ?infinity.

-type error_handler() :: fun(( jesse_error:error_reason()
                             , [jesse_error:error_reason()]
                             , non_neg_integer()
                             ) -> list()
                                | no_return()
                                ).

-type error_list() :: list().

%% -type external_validator() :: fun((json_term(), state()) -> state())
-type external_validator() :: fun((json_term(), any()) -> any())
                            | undefined.

%% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L57
-type http_uri_uri() :: string() | unicode:unicode_binary().

-type json_term() :: term().

-type parser_fun() :: fun((json_term() | binary()) -> json_term()).

-type schema() :: json_term().

-type schema_id() :: http_uri_uri() | undefined.

-type schema_ref() :: binary().

-type schema_ver() :: binary().

-type schema_loader_fun() :: fun((string()) -> {ok, schema()}
                                             | schema()
                                             | ?not_found
                                             ).

-type option() :: {allowed_errors, allowed_errors()}
                | {default_schema_ver, schema_ver()}
                | {error_handler, error_handler()}
                | {external_validator, external_validator()}
                | {meta_schema_ver, schema_ver()}
                | {parser_fun, parser_fun()}
                | {schema_loader_fun, schema_loader_fun()}.

-type options() :: [option()].

-type validation_fun() :: fun((any()) -> boolean()).

%%% API

%% @doc Run from CLI with arguments.
-spec main([string()]) -> ok.
main(Args) ->
  jesse_cli:main(Args).

%% @doc Adds a schema definition `Schema' to in-memory storage associated with
%% a key `Key'. It will overwrite an existing schema with the same key if
%% there is any.
-spec add_schema( Key :: string()
                , Schema :: schema()
                ) -> ok
                   | jesse_error:error().
add_schema(Key, Schema) ->
  ValidationFun = fun jesse_lib:is_json_object/1,
  jesse_database:add(Key, Schema, ValidationFun).

%% @doc Equivalent to `add_schema/2', but `Schema' is a binary string, and
%% the third agument is a parse function to convert the binary string to
%% a supported internal representation of json.
-spec add_schema( Key :: string()
                , Schema :: binary()
                , Options :: options()
                ) -> ok
                   | jesse_error:error().
add_schema(Key, Schema, Options) ->
  try
    ParserFun = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedSchema = try_parse(schema, ParserFun, Schema),
    add_schema(Key, ParsedSchema)
  catch
    throw:Error ->
      {error, Error}
  end.


%% @doc Deletes a schema definition from in-memory storage associated with
%% the key `Key'.
-spec del_schema(Key :: string()) -> ok.
del_schema(Key) ->
  jesse_database:delete(Key).

%% @doc Loads schema definitions from filesystem to in-memory storage.
%%
%% Equivalent to `load_schemas(Path, ParserFun, ValidationFun)'
%% where `ValidationFun' is `fun jesse_json:is_json_object/1'.
-spec load_schemas( Path :: string()
                  , ParserFun :: fun((binary()) -> json_term())
                  ) -> jesse_database:update_result().
load_schemas(Path, ParserFun) ->
  load_schemas( Path
              , ParserFun
              , fun jesse_lib:is_json_object/1
              ).

%% @doc Loads schema definitions from filesystem to in-memory storage.
%% The function loads all the files from directory `Path', then each schema
%% entry will be checked for a validity by function `ValidationFun', and
%% will be stored in in-memory storage.
%%
%% In addition to a schema definition, a timestamp of the schema file will be
%% stored, so, during the next update timestamps will be compared to avoid
%% unnecessary updates.
%%
%% Schema definitions are stored in the format which json parsing function
%% `ParserFun' returns.
%%
%% NOTE: it's impossible to automatically update schema definitions added by
%%       add_schema/2, the only way to update them is to use add_schema/2
%%       again with the new definition.
-spec load_schemas( Path :: string()
                  , ParserFun :: parser_fun()
                  , ValidationFun :: validation_fun()
                  ) -> jesse_database:update_result().
load_schemas(Path, ParserFun, ValidationFun) ->
  jesse_database:add_path(Path, ParserFun, ValidationFun).

%% @doc Equivalent to {@link validate/3} where `Options' is an empty list.
-spec validate( Schema :: schema() | binary()
              , Data :: json_term() | binary()
              ) -> {ok, json_term()}
                 | jesse_error:error()
                 | jesse_database:error().
validate(Schema, Data) ->
  validate(Schema, Data, []).

%% @doc Validates json `Data' against a schema with the same key as `Schema'
%% in the internal storage, using `Options'. If the given json is valid,
%% then it is returned to the caller, otherwise an error with an appropriate
%% error reason is returned. If the `parser_fun' option is provided, then
%% `Data' is considered to be a binary string, so `parser_fun' is used
%% to convert the binary string to a supported internal representation of json.
%% If `parser_fun' is not provided, then `Data' is considered to already be a
%% supported internal representation of json.
-spec validate( Schema :: schema() | binary()
              , Data :: json_term() | binary()
              , Options :: options()
              ) -> {ok, json_term()}
                 | jesse_error:error()
                 | jesse_database:error().
validate(Schema, Data, Options) ->
  try
    ParserFun = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedData = try_parse(data, ParserFun, Data),
    JsonSchema = jesse_database:load(Schema),
    jesse_schema_validator:validate(JsonSchema, ParsedData, Options)
  catch
    throw:Error ->
      {error, Error}
  end.

%% @doc Equivalent to {@link validate_definition/4} where `Options' is an empty list.
-spec validate_definition( Definition :: string()
                         , Schema     :: json_term() | binary()
                         , Data       :: json_term() | binary()
                         ) -> {ok, json_term()}
                            | jesse_error:error().
validate_definition(Definition, Schema, Data) ->
  validate_definition(Definition, Schema, Data, []).

%% @doc Validates json `Data' agains the given `Definition' in the given
%% schema `Schema', using `Options'.
%% If the given json is valid, then it is returned to the caller, otherwise
%% an error with an appropriate error reason is returned. If the `parser_fun'
%% option is provided, then both `Schema' and `Data' are considered to be a
%% binary string, so `parser_fun' is used to convert both binary strings to a
%% supported internal representation of json.
%% If `parser_fun' is not provided, then both `Schema' and `Data' are considered
%% to already be a supported internal representation of json.
-spec validate_definition( Definition :: string()
                         , Schema     :: json_term() | binary()
                         , Data       :: json_term() | binary()
                         , Options    :: [{Key :: atom(), Data :: any()}]
                         ) -> {ok, json_term()}
                            | jesse_error:error().
validate_definition(Defintion, Schema, Data, Options) ->
  try
    ParserFun    = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedSchema = try_parse(schema, ParserFun, Schema),
    ParsedData   = try_parse(data, ParserFun, Data),
    jesse_schema_validator:validate_definition( Defintion
                                              , ParsedSchema
                                              , ParsedData
                                              , Options
                                              )
  catch
    throw:Error -> {error, Error}
  end.

%% @doc Equivalent to {@link validate_local_ref/4} where `Options' is an empty list.
-spec validate_local_ref( RefPath :: string()
                         , Schema     :: json_term() | binary()
                         , Data       :: json_term() | binary()
                         ) -> {ok, json_term()}
                            | jesse_error:error().
validate_local_ref(RefPath, Schema, Data) ->
  validate_local_ref(RefPath, Schema, Data, []).

%% @doc Validates json `Data' agains the given definition path 'RefPath' in the given
%% schema `Schema', using `Options'.
%% If the given json is valid, then it is returned to the caller, otherwise
%% an error with an appropriate error reason is returned. If the `parser_fun'
%% option is provided, then both `Schema' and `Data' are considered to be a
%% binary string, so `parser_fun' is used to convert both binary strings to a
%% supported internal representation of json.
%% If `parser_fun' is not provided, then both `Schema' and `Data' are considered
%% to already be a supported internal representation of json.
-spec validate_local_ref( RefPath :: string()
                         , Schema     :: json_term() | binary()
                         , Data       :: json_term() | binary()
                         , Options    :: [{Key :: atom(), Data :: any()}]
                         ) -> {ok, json_term()}
                            | jesse_error:error().
validate_local_ref(RefPath, Schema, Data, Options) ->
  try
    ParserFun    = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedSchema = try_parse(schema, ParserFun, Schema),
    ParsedData   = try_parse(data, ParserFun, Data),
    jesse_schema_validator:validate_local_ref( RefPath
                                              , ParsedSchema
                                              , ParsedData
                                              , Options
                                              )
  catch
    throw:Error -> {error, Error}
  end.

%% @doc Equivalent to {@link validate_with_schema/3} where `Options'
%% is an empty list.
-spec validate_with_schema( Schema :: schema() | binary()
                          , Data :: json_term() | binary()
                          ) -> {ok, json_term()}
                             | jesse_error:error().
validate_with_schema(Schema, Data) ->
  validate_with_schema(Schema, Data, []).

%% @doc Validates json `Data' agains the given schema `Schema', using `Options'.
%% If the given json is valid, then it is returned to the caller, otherwise
%% an error with an appropriate error reason is returned. If the `parser_fun'
%% option is provided, then both `Schema' and `Data' are considered to be a
%% binary string, so `parser_fun' is used to convert both binary strings to a
%% supported internal representation of json.
%% If `parser_fun' is not provided, then both `Schema' and `Data' are considered
%% to already be a supported internal representation of json.
-spec validate_with_schema( Schema :: schema() | binary()
                          , Data :: json_term() | binary()
                          , Options :: options()
                          ) -> {ok, json_term()}
                             | jesse_error:error().
validate_with_schema(Schema, Data, Options) ->
  try
    ParserFun = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedSchema = try_parse(schema, ParserFun, Schema),
    ParsedData = try_parse(data, ParserFun, Data),
    jesse_schema_validator:validate(ParsedSchema, ParsedData, Options)
  catch
    throw:Error -> {error, Error}
  end.

%%% Internal functions
%% @doc Wraps up calls to a third party json parser.
%% @private
try_parse(Type, ParserFun, JsonBin) ->
  try
    ParserFun(JsonBin)
  catch
    _:Error ->
      case Type of
        data ->
          throw([{?data_error, {parse_error, Error}}]);
        schema ->
          throw([{?schema_error, {parse_error, Error}}])
      end
  end.
