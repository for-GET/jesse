%%%=============================================================================
%% Copyright 2024- AUTHORS
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
%% @doc jesse test suite which covers Draft 2020-12. It uses the official
%% JSON-Schema-Test-Suite
%% (https://github.com/json-schema/JSON-Schema-Test-Suite) as the test data.
%%
%% Groups/cases not yet supported by jesse are enumerated in the skip-list
%% below (with the reason) rather than being silently dropped. See the module
%% doc of `jesse_validator_draft2020_12' for the safety rationale.
%% @end
%%%=============================================================================

-module(jesse_tests_draft2020_12_SUITE).

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

-define(json_schema_draft2020_12,
        <<"https://json-schema.org/draft/2020-12/schema">>).

all() ->
  Exports = ?MODULE:module_info(exports),
  %% Test cases are the arity-1 exported functions; helpers like skip_list/0
  %% are excluded by the arity guard.
  [F || {F, 1} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(jesse),
  get_tests("standard", ?json_schema_draft2020_12, Config)
    ++ [{skip_list, skip_list()}]
    ++ Config.

end_per_suite(_Config) ->
  ok.

%% @doc Cases that jesse does not yet handle for draft 2020-12.
%% `{File, '_'}' skips every case in a file; `{File, Description}' skips one.
%% Each entry is deliberate: silently ignoring an unsupported keyword could
%% false-accept invalid data, so unsupported keywords hard-error and their
%% test groups are skip-listed here instead.
skip_list() ->
    %% "$dynamicRef"/"$dynamicAnchor": needs a dynamic scope stack (deferred).
    %% "$dynamicRef" hard-errors so it cannot false-accept.
  [ {<<"dynamicRef">>, '_'}
    %% Remote-schema fetching harness not wired for this dialect yet.
  , {<<"refRemote">>, '_'}
  , {<<"ref">>, <<"remote ref, containing refs itself">>}
  , {<<"ref">>, <<"Recursive references between schemas">>}
    %% "$defs" against the metaschema needs the remote 2020-12 metaschema
    %% (which itself uses "$dynamicRef").
  , {<<"defs">>, <<"validate definition against metaschema">>}
    %% "$anchor" resolution across an "$id" base-URI change (in-document remote
    %% scope map) is deferred; the local-anchor cases are supported.
  , {<<"anchor">>, <<"Location-independent identifier with absolute URI">>}
  , {<<"anchor">>, <<"Location-independent identifier with base URI change"
                     " in subschema">>}
    %% Every id.json case but the last validates a schema document against the
    %% remote 2020-12 metaschema; the last needs an in-document "$id" scope map.
  , {<<"id">>, '_'}
    %% "$id" buried in an unknown keyword: needs in-document "$id" scoping.
  , {<<"unknownKeyword">>, <<"$id inside an unknown keyword is not a"
                             " real identifier">>}
  ].

%%% Testcases (one per keyword file in tests/draft2020-12)

additionalProperties(Config) ->
  do_test("additionalProperties", Config).

allOf(Config) ->
  do_test("allOf", Config).

anchor(Config) ->
  do_test("anchor", Config).

anyOf(Config) ->
  do_test("anyOf", Config).

boolean_schema(Config) ->
  do_test("boolean_schema", Config).

const(Config) ->
  do_test("const", Config).

contains(Config) ->
  do_test("contains", Config).

content(Config) ->
  do_test("content", Config).

default(Config) ->
  do_test("default", Config).

defs(Config) ->
  do_test("defs", Config).

dependentRequired(Config) ->
  do_test("dependentRequired", Config).

dependentSchemas(Config) ->
  do_test("dependentSchemas", Config).

dynamicRef(Config) ->
  do_test("dynamicRef", Config).

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

'if-then-else'(Config) ->
  do_test("if-then-else", Config).

'infinite-loop-detection'(Config) ->
  do_test("infinite-loop-detection", Config).

items(Config) ->
  do_test("items", Config).

maxContains(Config) ->
  do_test("maxContains", Config).

maximum(Config) ->
  do_test("maximum", Config).

maxItems(Config) ->
  do_test("maxItems", Config).

maxLength(Config) ->
  do_test("maxLength", Config).

maxProperties(Config) ->
  do_test("maxProperties", Config).

minContains(Config) ->
  do_test("minContains", Config).

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

prefixItems(Config) ->
  do_test("prefixItems", Config).

properties(Config) ->
  do_test("properties", Config).

propertyNames(Config) ->
  do_test("propertyNames", Config).

ref(Config) ->
  do_test("ref", Config).

refRemote(Config) ->
  do_test("refRemote", Config).

required(Config) ->
  do_test("required", Config).

type(Config) ->
  do_test("type", Config).

unevaluatedItems(Config) ->
  do_test("unevaluatedItems", Config).

unevaluatedProperties(Config) ->
  do_test("unevaluatedProperties", Config).

uniqueItems(Config) ->
  do_test("uniqueItems", Config).

unknownKeyword(Config) ->
  do_test("unknownKeyword", Config).
