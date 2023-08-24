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
%% @doc EUnit tests for jesse_schema_validator.
%% @end
%%%=============================================================================

-module(jesse_schema_validator_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("jesse_schema_validator.hrl").


test_all_drafts(Fn) ->
    [ Fn(URI)
      || URI <- [<<"http://json-schema.org/draft-03/schema#">>,
                 <<"http://json-schema.org/draft-04/schema#">>,
                 <<"http://json-schema.org/draft-06/schema#">>] ].


data_invalid_test() ->
    test_all_drafts(fun data_invalid_test_draft/1).


data_invalid_test_draft(URI) ->
    IntegerSchema = {[{<<"$schema">>, URI},
                      {<<"type">>, <<"integer">>}]},

    %% A case without errors
    ?assertEqual(
      {ok, 42},
      jesse_schema_validator:validate(IntegerSchema, 42, [])),

    %% A schema for testing properties and patternProperties
    Schema = {[{<<"$schema">>, URI},
               {<<"type">>, <<"object">>},
               {<<"properties">>,
                {[{<<"foo">>,
                   {[{<<"type">>, <<"object">>},
                     {<<"properties">>,
                      {[{<<"subfoo">>, IntegerSchema}]}}]}}]}},
               {<<"patternProperties">>,
                {[{<<"^b">>, IntegerSchema}]}}]},

    %% Root level error
    ?assertThrow(
      [{data_invalid, Schema, wrong_type, <<"foo">>, []}],
      jesse_schema_validator:validate(Schema, <<"foo">>, [])),

    %% Properties, 2 levels
    ?assertThrow(
      [{data_invalid,
        IntegerSchema,
        wrong_type,
        <<"bar">>,
        [<<"foo">>, <<"subfoo">>]}],
      jesse_schema_validator:validate(Schema,
                                      {[{<<"foo">>,
                                         {[{<<"subfoo">>, <<"bar">>}]}}]},
                                      [])),

    %% patternProperties, level 1
    ?assertThrow(
      [{data_invalid, IntegerSchema, wrong_type, <<"baz">>, [<<"bar">>]}],
      jesse_schema_validator:validate(Schema, {[{<<"bar">>, <<"baz">>}]}, [])),

    %% Object, additionalProperties, level 1
    Schema2 = {[{<<"$schema">>, URI},
                {<<"type">>, <<"object">>},
                {<<"properties">>,
                 {[{<<"foo">>, IntegerSchema}]}},
                {<<"additionalProperties">>, false}]},

    %% additionalProperties, level1
    ?assertThrow(
      [{data_invalid, Schema2, no_extra_properties_allowed, _, [<<"bar">>]}],
      jesse_schema_validator:validate(Schema2,
                                      {[{<<"foo">>, 0},
                                        {<<"bar">>, <<"baz">>}]},
                                      [])),

    %% Object, additionalProperties, level 2
    Schema3 = {[{<<"$schema">>, URI},
                {<<"type">>, <<"object">>},
                {<<"properties">>,
                 {[{<<"foo">>,
                    {[{<<"type">>, <<"object">>},
                      {<<"properties">>,
                       {[{<<"subfoo">>, IntegerSchema}]}},
                      {<<"additionalProperties">>, false}]}}]}},
                {<<"additionalProperties">>, false}]},

    %% additionalProperties, level 2
    ?assertThrow(
      [{data_invalid, _, no_extra_properties_allowed, _, [<<"foo">>, <<"bar">>]}],
      jesse_schema_validator:validate(Schema3,
                                      {[{<<"foo">>,
                                         {[{<<"subfoo">>, 1},
                                           {<<"bar">>, 2}]}}]},
                                      [])),

    %% Items: A zero-based index is used in the property path
    ItemsSchema = {[{<<"$schema">>, URI},
                    {<<"type">>, <<"array">>},
                    {<<"items">>, IntegerSchema},
                    {<<"maxItems">>, 3}]},
    ?assertThrow(
      [{data_invalid, IntegerSchema, wrong_type, <<"baz">>, [1]}],
      jesse_schema_validator:validate(ItemsSchema, [2, <<"baz">>, 3], [])),
    ?assertThrow(
      [{data_invalid, ItemsSchema, wrong_size, [2, 3, 4, 5], []}],
      jesse_schema_validator:validate(ItemsSchema, [2, 3, 4, 5], [])),

    %% Items, a schema per item
    ItemsSchema2 = {[{<<"$schema">>, URI},
                     {<<"type">>, <<"array">>},
                     {<<"items">>, [IntegerSchema, IntegerSchema, IntegerSchema]},
                     {<<"additionalItems">>, false}]},
    ?assertThrow(
      [{data_invalid, IntegerSchema, wrong_type, <<"baz">>, [2]}],
      jesse_schema_validator:validate(ItemsSchema2, [2, 3, <<"baz">>], [])),
    ?assertThrow(
      [{data_invalid, ItemsSchema2, no_extra_items_allowed, [2, 3, 4, 5], []}],
      jesse_schema_validator:validate(ItemsSchema2, [2, 3, 4, 5], [])),

    %% Dependencies
    DependenciesSchema = {[{<<"$schema">>, URI},
                           {<<"type">>, <<"object">>},
                           {<<"dependencies">>,
                            {[{<<"bar">>, [<<"foo">>]}  %% if there is bar, there must also be foo
                             ]}}]},
    ?assertThrow(
      [{data_invalid,
        DependenciesSchema,
        {missing_dependency, <<"foo">>},
        {[{<<"bar">>, 42}]},
        []}],
      jesse_schema_validator:validate(DependenciesSchema,
                                      {[{<<"bar">>, 42}]},
                                      [])),

    ok.


dots_used_in_keys_test() ->
    test_all_drafts(fun dots_used_in_keys_test_draft/1).


dots_used_in_keys_test_draft(URI) ->
    Schema = {[{<<"$schema">>, URI},
               {<<"type">>, <<"object">>},
               {<<"properties">>,
                {[{<<"3.4.5.6.7">>, {[{<<"type">>, <<"string">>}]}},
                  {<<"additionalProperties">>, false}]}}]},
    ValidJson = {[{<<"3.4.5.6.7">>, <<"Hello world!">>}]},
    InvalidJson = {[{<<"3.4.5.6.7">>, true}]},

    ?assertEqual({ok, ValidJson},
                 jesse_schema_validator:validate(Schema, ValidJson, [])),
    ?assertThrow([{data_invalid,
                   {[{<<"type">>, <<"string">>}]},
                   wrong_type,
                   true,
                   [<<"3.4.5.6.7">>]}],
                 jesse_schema_validator:validate(Schema, InvalidJson, [])).


empty_list_as_valid_value_for_string_test() ->
    test_all_drafts(fun empty_list_as_valid_value_for_string_test_draft/1).


empty_list_as_valid_value_for_string_test_draft(URI) ->
    StringSchema = {[{<<"type">>, <<"string">>}]},

    EmptyListSchema = {[{<<"$schema">>, URI},
                        {<<"type">>, <<"object">>},
                        {<<"properties">>,
                         {[{<<"foo">>, StringSchema}]}}]},
    ?assertThrow(
      [{data_invalid, StringSchema, wrong_type, [], [<<"foo">>]}],
      jesse_schema_validator:validate(EmptyListSchema, [{<<"foo">>, []}], [])).


schema_unsupported_test() ->
    test_all_drafts(fun schema_unsupported_test_draft/1).


schema_unsupported_test_draft(URI) ->
    SupportedSchema = {[{<<"$schema">>,
                         URI}]},
    UnsupportedSchema = {[{<<"$schema">>,
                           <<"http://json-schema.org/draft-05/schema#">>}]},

    Json = {[{<<"Doesn't matter">>}]},
    ?assertEqual({ok, Json},
                 jesse_schema_validator:validate(SupportedSchema, Json, [])),
    ?assertThrow([{schema_invalid,
                   UnsupportedSchema,
                   {schema_unsupported,
                    <<"http://json-schema.org/draft-05/schema#">>}}],
                 jesse_schema_validator:validate(UnsupportedSchema, Json, [])).


data_invalid_one_of_test() ->
    [ data_invalid_one_of_test_draft(URI)
      || URI <- [<<"http://json-schema.org/draft-04/schema#">>,
                 <<"http://json-schema.org/draft-06/schema#">>] ].


data_invalid_one_of_test_draft(URI) ->
    IntegerSchema = {[{<<"type">>, <<"integer">>}]},
    StringSchema = {[{<<"type">>, <<"string">>}]},
    ObjectSchema = {[{<<"type">>, <<"object">>},
                     {<<"properties">>,
                      [{<<"name">>, StringSchema},
                       {<<"age">>, IntegerSchema}]},
                     {<<"additionalProperties">>, false}]},

    Schema = {[{<<"$schema">>, URI},
               {<<"oneOf">>, [IntegerSchema, StringSchema, ObjectSchema]}]},

    Json = [{<<"name">>, 42},
            {<<"age">>, <<"John">>}],

    ?assertThrow(
      [{data_invalid,
        Schema,
        {not_one_schema_valid,
         [{data_invalid, IntegerSchema, wrong_type, Json, []},
          {data_invalid, StringSchema, wrong_type, Json, []},
          {data_invalid, StringSchema, wrong_type, 42, [<<"name">>]}]},
        Json,
        []}],
      jesse_schema_validator:validate(Schema, Json, [])).


data_invalid_any_of_test() ->
    [ data_invalid_any_of_test_draft(URI)
      || URI <- [<<"http://json-schema.org/draft-04/schema#">>,
                 <<"http://json-schema.org/draft-06/schema#">>] ].


data_invalid_any_of_test_draft(URI) ->
    IntegerSchema = {[{<<"type">>, <<"integer">>}]},
    StringSchema = {[{<<"type">>, <<"string">>}]},
    ObjectSchema = {[{<<"type">>, <<"object">>},
                     {<<"properties">>,
                      [{<<"name">>, StringSchema},
                       {<<"age">>, IntegerSchema}]},
                     {<<"additionalProperties">>, false}]},

    Schema = {[{<<"$schema">>, URI},
               {<<"anyOf">>, [IntegerSchema, StringSchema, ObjectSchema]}]},

    Json = [{<<"name">>, 42},
            {<<"age">>, <<"John">>}],

    ?assertThrow(
      [{data_invalid,
        Schema,
        {any_schemas_not_valid,
         [{data_invalid, IntegerSchema, wrong_type, Json, []}]},
        Json,
        []}],
      jesse_schema_validator:validate(Schema, Json, [])).


-ifndef(erlang_deprecated_types).
-ifndef(COMMON_TEST).  % see Emakefile


map_schema_test() ->
    test_all_drafts(fun map_schema_test_draft/1).


map_schema_test_draft(URI) ->
    Schema = #{
               <<"$schema">> => URI,
               <<"type">> => <<"object">>,
               <<"properties">> =>
                   #{
                     <<"foo">> =>
                         #{
                           <<"type">> => <<"object">>,
                           <<"properties">> =>
                               #{
                                 <<"subfoo">> => #{
                                                   <<"type">> => <<"integer">>
                                                  }
                                }
                          }
                    },
               <<"patternProperties">> =>
                   #{
                     <<"^b">> => #{
                                   <<"type">> => <<"integer">>
                                  }
                    }
              },
    ValidJson = {[{<<"foo">>, {[{<<"subfoo">>, 42}]}},
                  {<<"bar">>, 42},
                  {<<"baz">>, 42}]},
    ?assertEqual({ok, ValidJson},
                 jesse_schema_validator:validate(Schema, ValidJson, [])),

    InvalidJson = {[{<<"bar">>, <<"str expect int">>}]},
    ?assertThrow([{data_invalid,
                   #{<<"type">> := <<"integer">>},
                   wrong_type,
                   <<"str expect int">>,
                   [<<"bar">>]}],
                 jesse_schema_validator:validate(Schema, InvalidJson, [])).


map_data_test() ->
    test_all_drafts(fun map_data_test_draft/1).


map_data_test_draft(URI) ->
    Schema = {[{<<"$schema">>, URI},
               {<<"type">>, <<"object">>},
               {<<"properties">>,
                {[{<<"foo">>,
                   {[{<<"type">>, <<"object">>},
                     {<<"properties">>,
                      {[{<<"subfoo">>,
                         {[{<<"type">>,
                            <<"integer">>}]}}]}}]}}]}},
               {<<"patternProperties">>,
                {[{<<"^b">>,
                   {[{<<"type">>, <<"integer">>}]}}]}}]},
    ValidJson = #{
                  <<"foo">> => #{<<"subfoo">> => 42},
                  <<"bar">> => 42,
                  <<"baz">> => 42
                 },
    ?assertEqual({ok, ValidJson},
                 jesse_schema_validator:validate(Schema, ValidJson, [])),

    InvalidJson = #{
                    <<"foo">> => 42,
                    <<"baz">> => #{}
                   },
    %% XXX: order of errors isn't guaranteed
    %% In case of future fails it can be replaced with manual catching and sorting
    %% of thrown error list, then checked using ?assertMatch
    ?assertThrow([{data_invalid,
                   {[{<<"type">>, <<"object">>} | _]},
                   wrong_type,
                   42,
                   [<<"foo">>]},
                  {data_invalid,
                   {[{<<"type">>, <<"integer">>}]},
                   wrong_type,
                   #{},
                   [<<"baz">>]}],
                 jesse_schema_validator:validate(Schema,
                                                 InvalidJson,
                                                 [{allowed_errors, infinity}])).


%% Make sure references work with schemas as maps
map_schema_references_test() ->
    Schema = #{
               <<"definitions">> =>
                   #{<<"a">> => #{<<"type">> => <<"object">>}},
               <<"type">> => <<"object">>,
               <<"properties">> =>
                   #{<<"prop">> => #{<<"$ref">> => <<"#/definitions/a">>}}
              },
    Json = #{<<"prop">> => #{}},
    ?assertMatch(
      {ok, _},
      jesse_schema_validator:validate(
        Schema,
        Json,
        [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}])).


data_exclusive_maximum_minimum_test() ->
    Schema = fun(Property, V) ->
                     {[{<<"$schema">>, V},
                       {<<"type">>, <<"number">>},
                       {Property, 43}]}
             end,
    ValidNumber = 42,
    %% A case without errors
    ?assertEqual(
      {ok, ValidNumber},
      jesse_schema_validator:validate(
        Schema(<<"exclusiveMaximum">>, ?json_schema_draft6), ValidNumber, [])),

    ?assertEqual(
      {ok, ValidNumber + 2},
      jesse_schema_validator:validate(
        Schema(<<"exclusiveMinimum">>, ?json_schema_draft6), ValidNumber + 2, [])),

    ?assertThrow([{data_invalid,
                   {[{<<"$schema">>, ?json_schema_draft6},
                     {<<"type">>, <<"number">>},
                     {<<"exclusiveMinimum">>, 43}]},
                   not_in_range,
                   42,
                   []}],
                 jesse_schema_validator:validate(
                   Schema(<<"exclusiveMinimum">>, ?json_schema_draft6),
                   ValidNumber,
                   [])),

    ?assertThrow([{data_invalid,
                   {[{<<"$schema">>, ?json_schema_draft6},
                     {<<"type">>, <<"number">>},
                     {<<"exclusiveMaximum">>, 43}]},
                   not_in_range,
                   44,
                   []}],
                 jesse_schema_validator:validate(
                   Schema(<<"exclusiveMaximum">>, ?json_schema_draft6),
                   ValidNumber + 2,
                   [])).


data_dollarid_test() ->
    SchemaWithId = fun(Draft, Id) ->
                           {[{<<"$schema">>, Draft},
                             {<<"type">>, <<"object">>},
                             {Id, <<"foo">>}]}
                   end,
    Object = {[{<<"foo">>, <<"bar">>}]},
    ?assertEqual(
      {ok, Object},
      jesse_schema_validator:validate(
        SchemaWithId(?json_schema_draft4, <<"id">>),
        Object,
        [])),

    ?assertThrow([{schema_invalid,
                   {[{<<"$schema">>,
                      <<"http://json-schema.org/draft-04/schema#">>},
                     {<<"type">>, <<"object">>},
                     {<<"$id">>, <<"foo">>}]},
                   wrong_draft4_id_tag}],
                 jesse_schema_validator:validate(
                   SchemaWithId(?json_schema_draft4, <<"$id">>), Object, [])),

    ?assertThrow([{schema_invalid,
                   {[{<<"$schema">>,
                      <<"http://json-schema.org/draft-06/schema#">>},
                     {<<"type">>, <<"object">>},
                     {<<"id">>, <<"foo">>}]},
                   wrong_draft6_id_tag}],
                 jesse_schema_validator:validate(
                   SchemaWithId(?json_schema_draft6, <<"id">>), Object, [])),

    ?assertEqual(
      {ok, Object},
      jesse_schema_validator:validate(
        SchemaWithId(?json_schema_draft6, <<"$id">>), Object, [])).


data_contains_test() ->
    Schema = {[{<<"$schema">>, ?json_schema_draft6},
               {<<"type">>, <<"array">>},
               {<<"contains">>,
                {[{<<"type">>, <<"number">>}]}}]},
    Array = [<<"foo">>, 42],
    ArrayOfString = [<<"foo">>, <<"bar">>],
    ?assertEqual(
      {ok, Array},
      jesse_schema_validator:validate(Schema, Array, [])),

    ?assertThrow([{data_invalid,
                   {[{<<"$schema">>, ?json_schema_draft6},
                     {<<"type">>, <<"array">>},
                     {<<"contains">>,
                      {[{<<"type">>, <<"number">>}]}}]},
                   data_invalid,
                   [<<"foo">>, <<"bar">>],
                   []}],
                 jesse_schema_validator:validate(Schema, ArrayOfString, [])).


data_const_test() ->
    Schema = {[{<<"$schema">>, ?json_schema_draft6},
               {<<"type">>, <<"string">>},
               {<<"const">>, <<"foo">>}]},

    ?assertEqual(
      {ok, <<"foo">>},
      jesse_schema_validator:validate(Schema, <<"foo">>, [])),
    ?assertThrow([{data_invalid,
                   {[{<<"$schema">>, ?json_schema_draft6},
                     {<<"type">>, <<"string">>},
                     {<<"const">>, <<"foo">>}]},
                   not_in_enum,
                   <<"bar">>,
                   []}],
                 jesse_schema_validator:validate(Schema, <<"bar">>, [])).


data_empty_required_test() ->
    Schema = {[{<<"$schema">>, ?json_schema_draft6},
               {<<"type">>, <<"object">>},
               {<<"required">>, []}]},

    ?assertEqual(
      {ok, {[]}},
      jesse_schema_validator:validate(Schema, {[]}, [])).


data_empty_dependencies_test() ->
    Schema = {[{<<"$schema">>, ?json_schema_draft6},
               {<<"type">>, <<"object">>},
               {<<"dependencies">>, []}]},

    ?assertEqual(
      {ok, {[]}},
      jesse_schema_validator:validate(Schema, {[]}, [])).


array_items_with_boolean_value_test() ->
    Schema = {[{<<"$schema">>, ?json_schema_draft6},
               {<<"type">>, <<"array">>},
               {<<"items">>, true}]},

    ?assertEqual(
      {ok, []},
      jesse_schema_validator:validate(Schema, [], [])),

    InvalidSchema = {[{<<"$schema">>, ?json_schema_draft6},
                      {<<"type">>, <<"array">>},
                      {<<"items">>, false}]},

    ?assertEqual({ok, []},
                 jesse_schema_validator:validate(InvalidSchema, [], [])),

    ?assertThrow([{data_invalid, _, not_schema_valid, 1, [0]}],
                 jesse_schema_validator:validate(InvalidSchema, [1], [])).


contains_with_boolean_value_test() ->
    Schema = {[{<<"$schema">>, ?json_schema_draft6},
               {<<"type">>, <<"array">>},
               {<<"contains">>, true}]},
    Array = [<<"foo">>, 42],
    ?assertEqual(
      {ok, Array},
      jesse_schema_validator:validate(Schema, Array, [])),

    InvalidSchema = {[{<<"$schema">>, ?json_schema_draft6},
                      {<<"type">>, <<"array">>},
                      {<<"contains">>, false}]},
    ?assertThrow([{data_invalid,
                   {[{<<"$schema">>,
                      <<"http://json-schema.org/draft-06/schema#">>},
                     {<<"type">>, <<"array">>},
                     {<<"contains">>, false}]},
                   data_invalid,
                   [],
                   []}],
                 jesse_schema_validator:validate(InvalidSchema, [], [])).


-endif.
-endif.
