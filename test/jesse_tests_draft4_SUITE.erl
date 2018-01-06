%%%=============================================================================
%% Copyright 2014- Klarna AB
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
%% @doc jesse test suite which covers Draft 04. It uses JSON-Schema-Test-Suite
%% (https://github.com/json-schema/JSON-Schema-Test-Suite) as the test data.
%% @end
%%%=============================================================================

-module(jesse_tests_draft4_SUITE).

-compile([ export_all
         , nowarn_export_all
         ]).

-define(EXCLUDED_FUNS, [ module_info
                       , all
                       , init_per_suite
                       , end_per_suite
                       ]).

-include_lib("common_test/include/ct.hrl").

-import(jesse_tests_util, [ get_tests/2
                          , do_test/2
                          ]).

all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

init_per_suite(Config) ->
  inets:start(),
  get_tests( "JSON-Schema-Test-Suite/tests/draft4"
           , <<"http://json-schema.org/draft-04/schema#">>
           )
    ++ get_tests( "JSON-Schema-Test-Suite-extra/draft4"
                , <<"http://json-schema.org/draft-04/schema#">>
                )
    ++ Config.

end_per_suite(_Config) ->
  inets:stop().

%%% Testcases

additionalItems(Config) ->
  do_test("additionalItems", Config).

additionalProperties(Config) ->
  do_test("additionalProperties", Config).

allOf(Config) ->
  do_test("allOf", Config).

anyOf(Config) ->
  do_test("anyOf", Config).

default(Config) ->
  do_test("default", Config).

definitions(Config) ->
  do_test("definitions", Config).

dependencies(Config) ->
  do_test("dependencies", Config).

enum(Config) ->
  do_test("enum", Config).

items(Config) ->
  do_test("items", Config).

maximum(Config) ->
  do_test("maximum", Config).

maxItems(Config) ->
  do_test("maxItems", Config).

maxLength(Config) ->
  do_test("maxLength", Config).

maxProperties(Config) ->
  do_test("maxProperties", Config).

minimum(Config) ->
  do_test("minimum", Config).

minItems(Config) ->
  do_test("minItems", Config).

minLength(Config) ->
  do_test("minLength", Config).

minProperties(Config) ->
  do_test("minProperties", Config).

multipleOf(Config) ->
  do_test("multipleOf", Config).

'not'(Config) ->
  do_test("not", Config).

oneOf(Config) ->
  do_test("oneOf", Config).

pattern(Config) ->
  do_test("pattern", Config).

patternProperties(Config) ->
  do_test("patternProperties", Config).

properties(Config) ->
  do_test("properties", Config).

ref(Config) ->
  do_test("ref", Config).

refRemote(Config) ->
  TestDir = os:getenv("TEST_DIR"),
  DocumentRoot = filename:join(TestDir, "JSON-Schema-Test-Suite/remotes"),
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

itemsExtra(Config) ->
  do_test("itemsExtra", Config).

remoteRefExtra(Config) ->
  do_test("remoteRefExtra", Config).

anyOfOneOfAllowedErrorsZeroExtra(Config) ->
  do_test("anyOfOneOfAllowedErrorsZeroExtra", Config).

anyOfOneOfAllowedErrorsOneExtra(Config) ->
  do_test("anyOfOneOfAllowedErrorsOneExtra", Config).

anyOfOneOfAllowedErrorsInfinityExtra(Config) ->
  do_test("anyOfOneOfAllowedErrorsInfinityExtra", Config).

unicodePatternProperties(Config) ->
  do_test("unicodePatternProperties", Config).

dateTimeFormat(Config) ->
  do_test("dateTimeFormat", Config).

ipv4Format(Config) ->
  do_test("ipv4Format", Config).

ipv6Format(Config) ->
  do_test("ipv6Format", Config).
