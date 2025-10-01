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
%%
%% @doc jesse test suite which covers Draft 03. It uses JSON-Schema-Test-Suite
%% (https://github.com/json-schema/JSON-Schema-Test-Suite) as the test data.
%% @end
%%%=============================================================================

-module(jesse_tests_draft3_SUITE).

-compile([ export_all
         , nowarn_export_all
         ]).

-define(EXCLUDED_FUNS, [ module_info
                       , all
                       , init_per_suite
                       , end_per_suite
                       ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(jesse_tests_util, [ get_tests/3
                          , do_test/2
                          ]).
-define(json_schema_draft3, <<"http://json-schema.org/draft-03/schema#">>).

all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(jesse),
  get_tests("standard", ?json_schema_draft3, Config)
    ++ get_tests("extra", ?json_schema_draft3, Config)
    ++ [{skip_list, []}]
    ++ Config.

end_per_suite(_Config) ->
  ok.

%%% Testcases

additionalItems(Config) ->
  do_test("additionalItems", Config).

additionalProperties(Config) ->
  do_test("additionalProperties", Config).

default(Config) ->
  do_test("default", Config).

dependencies(Config) ->
  do_test("dependencies", Config).

disallow(Config) ->
  do_test("disallow", Config).

divisibleBy(Config) ->
  do_test("divisibleBy", Config).

enum(Config) ->
  do_test("enum", Config).

extends(Config) ->
  do_test("extends", Config).

items(Config) ->
  do_test("items", Config).

maximum(Config) ->
  do_test("maximum", Config).

maxItems(Config) ->
  do_test("maxItems", Config).

maxLength(Config) ->
  do_test("maxLength", Config).

minimum(Config) ->
  do_test("minimum", Config).

minItems(Config) ->
  do_test("minItems", Config).

minLength(Config) ->
  do_test("minLength", Config).

pattern(Config) ->
  do_test("pattern", Config).

patternProperties(Config) ->
  do_test("patternProperties", Config).

properties(Config) ->
  do_test("properties", Config).

ref(Config) ->
  do_test("ref", Config).

refRemote(Config) ->
  TestDir = ?config(data_dir, Config),
  DocumentRoot = filename:join(TestDir, "remotes"),
  ServerOpts = [ {port, 1234}
               , {server_name, "localhost"}
               , {server_root, "."}
               , {document_root, DocumentRoot}
               ],
  inets:start(httpd, ServerOpts),
  do_test("refRemote", Config).

required(Config) ->
  do_test("required", Config).

type(Config) ->
  do_test("type", Config).

uniqueItems(Config) ->
  do_test("uniqueItems", Config).

%% Extra

%% The original bug originated from starting from a map schema input, so it was
%% not triggered by `do_test', which loads the schema as proplists rather than
%% maps.
extends_smoke_test(_Config) ->
  Schema = #{
             <<"$schema">> => <<"http://json-schema.org/draft-03/schema#">>,
             <<"description">> => <<"a description">>,
             <<"extends">> =>
               #{<<"properties">> =>
                   #{<<"disallow">> =>
                       #{<<"disallow">> => [<<"number">>],<<"required">> => true}}},
             <<"id">> => <<"http://json-schema.org/draft-03/schema#">>,
             <<"title">> => <<"title">>,
             <<"type">> => <<"object">>},
  Data = #{<<"disallow">> => <<"a">>},
  ?assertEqual({ok, Data}, jesse:validate_with_schema(Schema, Data)).

extendsExtra(Config) ->
  do_test("extendsExtra", Config).

itemsExtra(Config) ->
  do_test("itemsExtra", Config).

remoteRefExtra(Config) ->
  do_test("remoteRefExtra", Config).

unicodePatternProperties(Config) ->
  do_test("unicodePatternProperties", Config).

uniqueItemsExtra(Config) ->
  do_test("uniqueItemsExtra", Config).
