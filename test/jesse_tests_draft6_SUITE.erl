%%%=============================================================================
%% Copyright 2022- AUTHORS
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
%% @doc jesse test suite which covers Draft 06. It uses JSON-Schema-Test-Suite
%% (https://github.com/json-schema/JSON-Schema-Test-Suite) as the test data.
%% @end
%%%=============================================================================

-module(jesse_tests_draft6_SUITE).

-compile([export_all,
          nowarn_export_all]).

-define(EXCLUDED_FUNS, [module_info,
                        all,
                        init_per_suite,
                        end_per_suite]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(jesse_tests_util,
        [get_tests/3,
         do_test/2]).


all() ->
    Exports = ?MODULE:module_info(exports),
    [ F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS) ].


init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jesse),
    SkipList =
        [{<<"ref">>, <<"Location-independent identifier">>},
         {<<"ref">>, <<"Recursive references between schemas">>},
         {<<"ref">>,
          <<"Location-independent identifier with base URI"
            " change in subschema">>},
         {<<"refRemote">>, <<"base URI change">>},
         {<<"refRemote">>, <<"base URI change - change folder">>},
         {<<"refRemote">>, <<"base URI change - change folder in subschema">>},
         {<<"refRemote">>, <<"root ref in remote ref">>}  %todo
         ,
         {<<"id">>, <<"id inside an enum is not a real identifier">>},
         {<<"unknownKeyword">>,
          <<"$id inside an unknown keyword is not a"
            " real identifier">>}],
    get_tests("standard",
              <<"http://json-schema.org/draft-06/schema#">>,
              Config) ++
    get_tests("extra",
              <<"http://json-schema.org/draft-06/schema#">>,
              Config) ++
    [{skip_list, SkipList}] ++
    Config.


end_per_suite(_Config) ->
    ok.


%%% Testcases


additionalItems(Config) ->
    do_test("additionalItems", Config).


additionalProperties(Config) ->
    do_test("additionalProperties", Config).


allOf(Config) ->
    do_test("allOf", Config).


anyOf(Config) ->
    do_test("anyOf", Config).


boolean_schema(Config) ->
    do_test("boolean_schema", Config).


const(Config) ->
    do_test("const", Config).


contains(Config) ->
    do_test("contains", Config).


default(Config) ->
    do_test("default", Config).


definitions(Config) ->
    do_test("definitions", Config).


dependencies(Config) ->
    do_test("dependencies", Config).


enum(Config) ->
    do_test("enum", Config).


exclusiveMaximum(Config) ->
    do_test("exclusiveMaximum", Config).


exclusiveMinimum(Config) ->
    do_test("exclusiveMinimum", Config).


format(Config) ->
    do_test("format", Config).


id(Config) ->
    do_test("id", Config).


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


propertyNames(Config) ->
    do_test("propertyNames", Config).


ref(Config) ->
    do_test("ref", Config).


refRemote(Config) ->
    TestDir = ?config(data_dir, Config),
    DocumentRoot = filename:join(TestDir, "remotes"),
    ServerOpts = [{port, 1234},
                  {server_name, "localhost"},
                  {server_root, "."},
                  {document_root, DocumentRoot}],
    inets:start(httpd, ServerOpts),
    do_test("refRemote", Config).


required(Config) ->
    do_test("required", Config).


type(Config) ->
    do_test("type", Config).


uniqueItems(Config) ->
    do_test("uniqueItems", Config).


uknownKeyword(Config) ->
    do_test("unknownKeyword", Config).

%% Extra
