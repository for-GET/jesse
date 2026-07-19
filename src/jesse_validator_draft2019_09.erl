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
%% @doc Json schema validation module for draft 2019-09.
%%
%% https://json-schema.org/draft/2019-09/json-schema-core.html
%% https://json-schema.org/draft/2019-09/json-schema-validation.html
%%
%% This module is forked from `jesse_validator_draft6'. The differences that
%% matter for 2019-09 are:
%%   * "$ref" is evaluated alongside sibling keywords instead of replacing
%%     them (draft 2019-09 dropped the "ref replaces the whole schema" rule).
%%   * "dependencies" was split into "dependentRequired" and "dependentSchemas".
%%   * "if"/"then"/"else" conditional application.
%%   * "contains" gained the "minContains"/"maxContains" bounds.
%%   * "$defs"/"$anchor" identifiers (resolution lives in `jesse_state').
%%   * "format" is annotation-only by default (non-asserting).
%%
%% Safety rule: a keyword that this dialect *defines* but jesse does not yet
%% implement (`unevaluatedProperties', `unevaluatedItems', `$recursiveRef')
%% raises `keyword_not_supported' rather than being silently ignored, so it can
%% never false-accept data the keyword would have rejected. Only genuinely
%% annotation-only keywords are ignored.
%% @end
%%%=============================================================================

-module(jesse_validator_draft2019_09).

%% API
-export([ check_value/3
        ]).

%% Includes
-include("jesse_schema_validator.hrl").


-type schema_error() :: ?invalid_dependency
                      | ?only_ref_allowed
                      | ?schema_invalid
                      | ?wrong_all_of_schema_array
                      | ?wrong_any_of_schema_array
                      | ?wrong_max_properties
                      | ?wrong_min_properties
                      | ?wrong_multiple_of
                      | ?wrong_one_of_schema_array
                      | ?wrong_required_array
                      | ?wrong_type_dependency
                      | ?wrong_type_items
                      | ?wrong_type_specification
                      | ?keyword_not_supported.

-type schema_error_type() :: schema_error()
                           | {schema_error(), jesse:json_term()}.

-type data_error() :: ?all_schemas_not_valid
                    | ?any_schemas_not_valid
                    | ?missing_dependency
                    | ?missing_required_property
                    | ?no_extra_items_allowed
                    | ?no_extra_properties_allowed
                    | ?no_match
                    | ?not_found
                    | ?not_in_enum
                    | ?not_in_range
                    | ?not_multiple_of
                    | ?not_one_schema_valid
                    | ?more_than_one_schema_valid
                    | ?not_schema_valid
                    | ?too_few_properties
                    | ?too_many_properties
                    | ?wrong_length
                    | ?wrong_size
                    | ?wrong_type
                    | ?external.

-type data_error_type() :: data_error()
                         | {data_error(), binary()}
                         | {data_error(), [jesse_error:error_reason()]}.

%%% API
%% @doc Goes through attributes of the given schema `JsonSchema' and
%% validates the value `Value' against them.
-spec check_value( Value :: jesse:json_term()
                 , JsonSchema :: jesse:schema()
                 , State :: jesse_state:state()
                 ) -> jesse_state:state() | no_return().
%% Draft 2019-09: "$ref" is evaluated alongside its sibling keywords rather than
%% replacing the schema, so we continue the keyword walk after resolving it.
check_value(Value, [{?REF, RefSchemaURI} | Attrs], State) ->
  NewState = validate_ref(Value, RefSchemaURI, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?TYPE, Type} | Attrs], State) ->
  NewState = check_type(Value, Type, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PROPERTIES, Properties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_properties( Value
                                        , unwrap(Properties)
                                        , State
                                        );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?PATTERNPROPERTIES, PatternProperties} | Attrs]
           , State
           ) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_pattern_properties( Value
                                                , PatternProperties
                                                , State
                                                );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?PROPERTYNAMES, PropertiesSchema} | Attrs]
           , State
           ) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_property_names( Value
                                            , canonical(PropertiesSchema)
                                            , State
                                            );
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value( Value
           , [{?ADDITIONALPROPERTIES, AdditionalProperties} | Attrs]
           , State
           ) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_additional_properties( Value
                                                   , AdditionalProperties
                                                   , State
                                                   );
               false -> State
       end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ITEMS, Items} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_items(Value, Items, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
%% doesn't really do anything, since this attribute will be handled
%% by the previous function clause if it's presented in the schema
check_value( Value
           , [{?ADDITIONALITEMS, _AdditionalItems} | Attrs]
           , State
           ) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?CONTAINS, Schema} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_contains(Value, Schema, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
%% "minContains"/"maxContains" are consumed together with "contains" (which
%% reads them off the current schema). Standalone, they have no effect.
check_value(Value, [{?MINCONTAINS, _} | Attrs], State) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?MAXCONTAINS, _} | Attrs], State) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?REQUIRED, Required} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_required(Value, Required, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?DEPENDENTREQUIRED, Dependencies} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_dependent_required(Value, Dependencies, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?DEPENDENTSCHEMAS, Dependencies} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_dependent_schemas(Value, Dependencies, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?IF, IfSchema} | Attrs], State) ->
  NewState = check_if_then_else(Value, canonical(IfSchema), State),
  check_value(Value, Attrs, NewState);
%% "then"/"else" are applied by the "if" clause above; alone they are inert.
check_value(Value, [{?THEN, _} | Attrs], State) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?ELSE, _} | Attrs], State) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?MINIMUM, Minimum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 check_minimum(Value, Minimum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?EXCLUSIVEMINIMUM, ExclusiveMinimum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 check_exclusive_minimum(Value, ExclusiveMinimum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXIMUM, Maximum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 check_maximum(Value, Maximum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?EXCLUSIVEMAXIMUM, ExclusiveMaximum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  ->
                 check_exclusive_maximum(Value, ExclusiveMaximum, State);
               false ->
                 State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINITEMS, MinItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_min_items(Value, MinItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXITEMS, MaxItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_max_items(Value, MaxItems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?UNIQUEITEMS, Uniqueitems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_unique_items(Value, Uniqueitems, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?PATTERN, Pattern} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_pattern(Value, Pattern, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINLENGTH, MinLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_min_length(Value, MinLength, State);
               false -> State
  end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXLENGTH, MaxLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_max_length(Value, MaxLength, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ENUM, Enum} | Attrs], State) ->
  NewState = check_enum(Value, Enum, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?CONST, Const} | Attrs], State) ->
  NewState = check_enum(Value, [Const], State),
  check_value(Value, Attrs, NewState);
%% Draft 2019-09 "format" is annotation-only by default (the format-assertion
%% vocabulary is opt-in and not implemented here), so it never asserts.
check_value(Value, [{?FORMAT, _Format} | Attrs], State) ->
  check_value(Value, Attrs, State);
check_value(Value, [{?MULTIPLEOF, Multiple} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_multiple_of(Value, Multiple, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MAXPROPERTIES, MaxProperties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_max_properties(Value, MaxProperties, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?MINPROPERTIES, MinProperties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_min_properties(Value, MinProperties, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ALLOF, Schemas} | Attrs], State) ->
  NewState = check_all_of(Value, Schemas, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ANYOF, Schemas} | Attrs], State) ->
  NewState = check_any_of(Value, Schemas, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?ONEOF, Schemas} | Attrs], State) ->
  NewState = check_one_of(Value, Schemas, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?NOT, Schema} | Attrs], State) ->
  NewState = check_not(Value, canonical(Schema), State),
  check_value(Value, Attrs, NewState);
%% Defined-but-not-yet-implemented keywords: surface an error instead of
%% silently ignoring them (which would false-accept). See J2/J3.
check_value(Value, [{?UNEVALUATEDPROPERTIES, _} | Attrs], State) ->
  NewState = unsupported_keyword(?UNEVALUATEDPROPERTIES, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?UNEVALUATEDITEMS, _} | Attrs], State) ->
  NewState = unsupported_keyword(?UNEVALUATEDITEMS, State),
  check_value(Value, Attrs, NewState);
check_value(Value, [{?RECURSIVEREF, _} | Attrs], State) ->
  NewState = unsupported_keyword(?RECURSIVEREF, State),
  check_value(Value, Attrs, NewState);
check_value(Value, Bool, State) when is_boolean(Bool) ->
  %% Boolean schemas: true always passes, false always fails.
  check_value(Value, unwrap(canonical(Bool)), State);
check_value(Value, [], State) ->
  maybe_external_check_value(Value, State);
%% Unknown keywords (including "$id", "$anchor", "$defs", "$comment",
%% "$vocabulary", "$recursiveAnchor", "definitions", the annotation/metadata
%% keywords, and any content-vocabulary keyword) carry no assertion and are
%% ignored, per spec. Identifier keywords are consumed by `jesse_state'.
check_value(Value, [_Attr | Attrs], State) ->
  check_value(Value, Attrs, State).

%%% Internal functions
%% @doc Raise a schema error for a keyword the dialect defines but which jesse
%% does not implement yet, so it surfaces instead of false-accepting.
%% @private
unsupported_keyword(Keyword, State) ->
  handle_schema_invalid({?keyword_not_supported, Keyword}, State).

%% @doc Adds Property to the current path and checks the value
%% using jesse_schema_validator:validate_with_state/3.
%% @private
check_value(Property, Value, Attrs, State) ->
  %% Add Property to path
  State1 = add_to_path(State, Property),
  State2 = jesse_schema_validator:validate_with_state(Attrs, Value, State1),
  %% Reset path again
  remove_last_from_path(State2).

%% @doc 5.5.2. type
%% @private
check_type(Value, Type, State) ->
  try
    IsValid = case jesse_lib:is_array(Type) of
                true  -> check_union_type(Value, Type, State);
                false -> is_type_valid(Value, Type)
              end,
    case IsValid of
      true  -> State;
      false -> wrong_type(Value, State)
    end
  catch
    %% The schema was invalid
    error:function_clause ->
      handle_schema_invalid(?wrong_type_specification, State)
  end.


%% @private
is_type_valid(Value, ?STRING)  -> is_binary(Value);
is_type_valid(Value, ?NUMBER)  -> is_number(Value);
is_type_valid(Value, ?INTEGER) when is_float(Value) ->
  (Value - trunc(Value)) == 0.0;
is_type_valid(Value, ?INTEGER) -> is_integer(Value);
is_type_valid(Value, ?BOOLEAN) -> is_boolean(Value);
is_type_valid(Value, ?OBJECT)  -> jesse_lib:is_json_object(Value);
is_type_valid(Value, ?ARRAY)   -> jesse_lib:is_array(Value);
is_type_valid(Value, ?NULL)    -> jesse_lib:is_null(Value).

%% @private
check_union_type(Value, [_ | _] = UnionType, _State) ->
  lists:any(fun(Type) -> is_type_valid(Value, Type) end, UnionType);
check_union_type(_Value, _InvalidTypes, State) ->
  handle_schema_invalid(?wrong_type_specification, State).

%% @private
wrong_type(Value, State) ->
  handle_data_invalid(?wrong_type, Value, State).


%% @doc properties
%% @private
check_properties(Value, Properties, State) ->
  TmpState
    = lists:foldl( fun({PropertyName, PropertySchema}, CurrentState) ->
                       case get_value(PropertyName, Value) of
                         ?not_found ->
                           CurrentState;
                         Property ->
                           NewState = set_current_schema(
                                        CurrentState
                                       , canonical(PropertySchema)),
                           check_value( PropertyName
                                      , Property
                                      , canonical(PropertySchema)
                                      , NewState
                                      )
                       end
                   end
                 , State
                 , Properties
                 ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @doc patternProperties
%% @private
check_pattern_properties(Value, PatternProperties, State) ->
  P1P2 = [{P1, P2} || P1 <- unwrap(Value),
                      P2  <- unwrap(PatternProperties)],
  TmpState = lists:foldl( fun({Property, Pattern}, CurrentState) ->
                              check_match(Property, Pattern, CurrentState)
                          end
                        , State
                        , P1P2
                        ),
  set_current_schema(TmpState, get_current_schema(State)).

check_property_names(Value, PropertiesSchema, State) ->
  SubState = set_current_schema(State , PropertiesSchema),
  TmpState = lists:foldl(
               fun({PropertyName, _Value}, CurrentState) ->
                   check_value( PropertyName
                              , PropertyName
                              , PropertiesSchema
                              , CurrentState)
               end
              , SubState
              , unwrap(Value)
              ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @private
check_match({PropertyName, PropertyValue}, {Pattern, Schema0}, State) ->
  Schema = canonical(Schema0),
  case jesse_lib:re_run(PropertyName, Pattern) of
    match   ->
      check_value( PropertyName
                 , PropertyValue
                 , Schema
                 , set_current_schema(State, Schema)
                 );
    nomatch ->
      State
  end.

%% @doc additionalProperties
%% @private
check_additional_properties(Value, false, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> State;
    Extras ->
      lists:foldl( fun({Property, _}, State1) ->
                       State2
                         = handle_data_invalid( ?no_extra_properties_allowed
                                              , Value
                                              , add_to_path(State1, Property)
                                              ),
                       remove_last_from_path(State2)
                   end
                 , State
                 , Extras
                 )
  end;
check_additional_properties(_Value, true, State) ->
  State;
check_additional_properties(Value, AdditionalProperties, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  case get_additional_properties(Value, Properties, PatternProperties) of
    []     -> State;
    Extras ->
      TmpState
        = lists:foldl( fun({ExtraName, Extra}, CurrentState) ->
                           NewState = set_current_schema( CurrentState
                                                        , AdditionalProperties
                                                        ),
                           check_value( ExtraName
                                      , Extra
                                      , AdditionalProperties
                                      , NewState
                                      )
                       end
                     , State
                     , Extras
                     ),
      set_current_schema(TmpState, JsonSchema)
  end.

%% @doc Returns the additional properties as a list of pairs containing the name
%% and the value of all properties not covered by Properties
%% or PatternProperties.
%% @private
get_additional_properties(Value, Properties, PatternProperties) ->
  ValuePropertiesNames  = [Name || {Name, _} <- unwrap(Value)],
  SchemaPropertiesNames = [Name || {Name, _} <- unwrap(Properties)],
  Patterns    = [Pattern || {Pattern, _} <- unwrap(PatternProperties)],
  ExtraNames0 = lists:subtract(ValuePropertiesNames, SchemaPropertiesNames),
  ExtraNames  = lists:foldl( fun(Pattern, ExtraAcc) ->
                                 filter_extra_names(Pattern, ExtraAcc)
                             end
                           , ExtraNames0
                           , Patterns
                           ),
  lists:map(fun(Name) -> {Name, get_value(Name, Value)} end, ExtraNames).

%% @private
filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case jesse_lib:re_run(ExtraName, Pattern) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc additionalItems and items
%% @private
check_items(Value, Items0, State) ->
  case jesse_lib:is_json_object(Items0) orelse is_boolean(Items0) of
    true ->
      Items = canonical(Items0),
      {_, TmpState} = lists:foldl( fun(Item, {Index, CurrentState}) ->
                                       { Index + 1
                                       , check_value( Index
                                                    , Item
                                                    , Items
                                                    , CurrentState
                                                    )
                                       }
                                   end
                                 , {0, set_current_schema(State, Items)}
                                 , Value
                                 ),
      set_current_schema(TmpState, get_current_schema(State));
    false when is_list(Items0) ->
      check_items_array(Value, lists:map(fun canonical/1, Items0), State);
    _ ->
      handle_schema_invalid({?wrong_type_items, Items0}, State)
  end.

%% @doc contains / minContains / maxContains
%%
%% An array is valid if the number of elements matching the "contains" schema is
%% at least "minContains" (default 1) and at most "maxContains" (default
%% unbounded). "minContains" of 0 makes "contains" trivially satisfied.
%% @private
check_contains(Values, Schema0, State) ->
  Schema     = canonical(Schema0),
  JsonSchema = get_current_schema(State),
  MinContains = contains_bound(get_value(?MINCONTAINS, JsonSchema), 1),
  MaxContains = contains_bound(get_value(?MAXCONTAINS, JsonSchema), ?infinity),
  MatchCount = count_contains_matches(Values, Schema, State),
  case in_contains_range(MatchCount, MinContains, MaxContains) of
    true  -> State;
    false -> handle_data_invalid(?data_invalid, Values, State)
  end.

%% @private
contains_bound(?not_found, Default) -> Default;
contains_bound(Value, _Default)     -> Value.

%% @private
in_contains_range(Count, Min, Max) ->
  Count >= Min andalso (Max =:= ?infinity orelse Count =< Max).

%% @private
count_contains_matches(Values, Schema, State) ->
  lists:foldl( fun(Value, Acc) ->
                   case validate_schema(Value, Schema, State) of
                     {true, _}  -> Acc + 1;
                     {false, _} -> Acc
                   end
               end
             , 0
             , Values
             ).

%% @private
check_items_array(Value, Items, State) ->
  JsonSchema = get_current_schema(State),
  NExtra = length(Value) - length(Items),
  case NExtra > 0 of
    true ->
      case get_value(?ADDITIONALITEMS, JsonSchema) of
        ?not_found -> State;
        true       -> State;
        false      ->
          handle_data_invalid(?no_extra_items_allowed, Value, State);
        AdditionalItems ->
          ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
          Tuples = lists:zip(Value, lists:append(Items, ExtraSchemas)),
          check_items_fun(Tuples, State)
      end;
    false ->
      RelevantItems = case NExtra of
                        0 ->
                          Items;
                        _ ->
                          lists:sublist(Items, length(Value))
                      end,
      check_items_fun(lists:zip(Value, RelevantItems), State)
  end.

%% @private
check_items_fun(Tuples, State) ->
  {_, TmpState} = lists:foldl( fun({Item, Schema}, {Index, CurrentState}) ->
                                 NewState = set_current_schema( CurrentState
                                                              , Schema
                                                              ),
                                 { Index + 1
                                 , check_value(Index, Item, Schema, NewState)
                                 }
                               end
                             , {0, State}
                             , Tuples
                             ),
  set_current_schema(TmpState, get_current_schema(State)).

%% @doc dependentRequired
%%
%% Object keyword. Value is a map of property-name -> array of property names
%% that must also be present when the key property is present.
%% @private
check_dependent_required(Value, Dependencies, State) ->
  lists:foldl( fun({DependencyName, RequiredNames}, CurrentState) ->
                   case get_value(DependencyName, Value) of
                     ?not_found -> CurrentState;
                     _          -> check_dependency_array( Value
                                                         , DependencyName
                                                         , RequiredNames
                                                         , CurrentState
                                                         )
                   end
               end
             , State
             , unwrap(Dependencies)
             ).

%% @doc dependentSchemas
%%
%% Object keyword. Value is a map of property-name -> subschema that the whole
%% instance must validate against when the key property is present.
%% @private
check_dependent_schemas(Value, Dependencies, State) ->
  lists:foldl( fun({DependencyName, DependencySchema}, CurrentState) ->
                   case get_value(DependencyName, Value) of
                     ?not_found -> CurrentState;
                     _          -> check_dependency_value(
                                     Value
                                    , DependencyName
                                    , canonical(DependencySchema)
                                    , CurrentState
                                    )
                   end
               end
             , State
             , unwrap(Dependencies)
             ).

%% @private
check_dependency_value(Value, DependencyName, Dependency, State) ->
  case jesse_lib:is_json_object(Dependency) of
    true ->
      TmpState = check_value( DependencyName
                            , Value
                            , Dependency
                            , set_current_schema(State, Dependency)
                            ),
      set_current_schema(TmpState, get_current_schema(State));
    false ->
      handle_schema_invalid({?wrong_type_dependency, Dependency}, State)
  end.

check_dependency(Value, Dependency, State)
  when is_binary(Dependency) ->
  case get_value(Dependency, Value) of
    ?not_found ->
      handle_data_invalid({?missing_dependency, Dependency}, Value, State);
    _          ->
      State
  end;
check_dependency(_Value, _Dependency, State) ->
    handle_schema_invalid(?invalid_dependency, State).

%% @private
check_dependency_array(Value, DependencyName, Dependency, State)
  when is_list(Dependency) ->
  lists:foldl( fun(PropertyName, CurrentState) ->
                   case get_value(DependencyName, Value) of
                       ?not_found ->
                         CurrentState;
                       _Exists ->
                         check_dependency( Value
                                         , PropertyName
                                         , CurrentState
                                         )
                   end
               end
             , State
             , Dependency
             );
check_dependency_array(_Value, _DependencyName, Dependency, State) ->
  handle_schema_invalid({?wrong_type_dependency, Dependency}, State).

%% @doc if / then / else
%%
%% If the instance validates against "if", it must validate against "then"
%% (when present); otherwise it must validate against "else" (when present).
%% "if" itself never produces validation errors of its own.
%% @private
check_if_then_else(Value, IfSchema, State) ->
  JsonSchema = get_current_schema(State),
  Branch = case validate_schema(Value, IfSchema, State) of
             {true, _}  -> ?THEN;
             {false, _} -> ?ELSE
           end,
  case get_value(Branch, JsonSchema) of
    ?not_found ->
      State;
    BranchSchema ->
      apply_branch(Value, canonical(BranchSchema), State)
  end.

%% @private
apply_branch(Value, Schema, State) ->
  case validate_schema(Value, Schema, State) of
    {true, NewState} -> NewState;
    {false, Errors}  ->
      handle_data_invalid({?not_schema_valid, Errors}, Value, State)
  end.

%% @doc minimum / exclusiveMinimum
%% @private
check_minimum(Value, Minimum, State) ->
  case (Value >= Minimum) of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.

check_exclusive_minimum(Value, ExclusiveMinimum, State) ->
  case (Value > ExclusiveMinimum) of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.


%% @doc maximum / exclusiveMaximum
%% @private
check_maximum(Value, Maximum, State) ->
  case (Value =< Maximum) of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.

check_exclusive_maximum(Value, ExclusiveMaximum, State) ->
  case (Value < ExclusiveMaximum) of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_range, Value, State)
  end.

%% @doc minItems
%% @private
check_min_items(Value, MinItems, State) when length(Value) >= MinItems ->
  State;
check_min_items(Value, _MinItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc maxItems
%% @private
check_max_items(Value, MaxItems, State) when length(Value) =< MaxItems ->
  State;
check_max_items(Value, _MaxItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc uniqueItems
%% @private
check_unique_items(_, false, State) ->
  State;
check_unique_items([], true, State) ->
  State;
check_unique_items([_], true, State) ->
  State;
check_unique_items(Value, true, State) ->
  try
    NormalizedValue = jesse_lib:normalize_and_sort(Value),
    NoDuplicates = ?SET_FROM_LIST(NormalizedValue),
    case sets:size(NoDuplicates) == length(Value) of
      true -> State;
      false ->
        lists:foldl( fun compare_rest_items/2
                   , tl(Value)
                   , Value
                   ),
        State
    end
  catch
    throw:ErrorInfo -> handle_data_invalid(ErrorInfo, Value, State)
  end.

%% @private
compare_rest_items(_Item, []) ->
  ok;
compare_rest_items(Item, RestItems) ->
  lists:foreach( fun(ItemFromRest) ->
                     case jesse_lib:is_equal(Item, ItemFromRest) of
                       true  -> throw({?not_unique, Item});
                       false -> ok
                     end
                 end
               , RestItems
               ),
  tl(RestItems).

%% @doc pattern
%% @private
check_pattern(Value, Pattern, State) ->
  case jesse_lib:re_run(Value, Pattern) of
    match   -> State;
    nomatch ->
      handle_data_invalid(?no_match, Value, State)
  end.

%% @doc minLength
%% @private
check_min_length(Value, MinLength, State) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc maxLength
%% @private
check_max_length(Value, MaxLength, State) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc enum / const
%% @private
check_enum(Value, Enum, State) ->
  IsValid = lists:any( fun(ExpectedValue) ->
                           jesse_lib:is_equal(Value, ExpectedValue)
                       end
                     , Enum
                     ),
  case IsValid of
    true  -> State;
    false ->
      handle_data_invalid(?not_in_enum, Value, State)
  end.

%% @doc multipleOf
%% @private
check_multiple_of(Value, MultipleOf, State)
  when is_number(MultipleOf), MultipleOf > 0 ->
  try (Value / MultipleOf - trunc(Value / MultipleOf)) * MultipleOf == 0.0 of
    true ->
      State;
    _   ->
      handle_data_invalid(?not_multiple_of, Value, State)
  catch error:badarith ->
      %% eg, division by zero or overflow
      handle_schema_invalid(?wrong_multiple_of, State)
  end;
check_multiple_of(_Value, _MultipleOf, State) ->
  handle_schema_invalid(?wrong_multiple_of, State).

%% @doc required
%% @private
check_required(Value, [] = Required, State) ->
  check_required_values(Value, Required, State);
check_required(Value, [_ | _] = Required, State) ->
  check_required_values(Value, Required, State);
check_required(_Value, _InvalidRequired, State) ->
  handle_schema_invalid(?wrong_required_array, State).

check_required_values(_Value, [], State) -> State;
check_required_values(Value, [PropertyName | Required], State) ->
  case get_value(PropertyName, Value) =/= ?not_found of
    'false' ->
      NewState =
        handle_data_invalid(?missing_required_property, PropertyName, State),
      check_required_values(Value, Required, NewState);
    'true' ->
      check_required_values(Value, Required, State)
  end.

%% @doc maxProperties
%% @private
check_max_properties(Value, MaxProperties, State)
  when is_integer(MaxProperties), MaxProperties >= 0 ->
    case length(unwrap(Value)) =< MaxProperties of
      true  -> State;
      false -> handle_data_invalid(?too_many_properties, Value, State)
    end;
check_max_properties(_Value, _MaxProperties, State) ->
  handle_schema_invalid(?wrong_max_properties, State).

%% @doc minProperties
%% @private
check_min_properties(Value, MinProperties, State)
  when is_integer(MinProperties), MinProperties >= 0 ->
    case length(unwrap(Value)) >= MinProperties of
      true  -> State;
      false -> handle_data_invalid(?too_few_properties, Value, State)
    end;
check_min_properties(_Value, _MaxProperties, State) ->
  handle_schema_invalid(?wrong_min_properties, State).

%% @doc allOf
%% @private
check_all_of(Value, [_ | _] = Schemas, State) ->
  check_all_of_(Value, Schemas, State);
check_all_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_all_of_schema_array, State).

check_all_of_(_Value, [], State) ->
  State;
check_all_of_(Value, [Schema | Schemas], State) ->
  case validate_schema(Value, Schema, State) of
    {true, NewState} ->
      check_all_of_(Value, Schemas, NewState);
    {false, Errors} ->
      handle_data_invalid({?all_schemas_not_valid, Errors}, Value, State)
  end.

%% @doc anyOf
%% @private
check_any_of(Value, [_ | _] = Schemas, State) ->
  check_any_of_(Value, Schemas, State, empty);
check_any_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_any_of_schema_array, State).

check_any_of_(Value, [], State, []) ->
  handle_data_invalid(?any_schemas_not_valid, Value, State);
check_any_of_(Value, [], State, Errors) ->
  handle_data_invalid({?any_schemas_not_valid, Errors}, Value, State);
check_any_of_(Value, [Schema | Schemas], State, Errors) ->
  ErrorsBefore = jesse_state:get_error_list(State),
  NumErrsBefore = length(ErrorsBefore),
  case validate_schema(Value, Schema, State) of
    {true, NewState} ->
      ErrorsAfter = jesse_state:get_error_list(NewState),
      case length(ErrorsAfter) of
        NumErrsBefore -> NewState;
        _  ->
          NewErrors = ErrorsAfter -- ErrorsBefore,
          check_any_of_(Value, Schemas, State, shortest(NewErrors, Errors))
      end;
    {false, NewErrors} ->
      check_any_of_(Value, Schemas, State, shortest(NewErrors, Errors))
  end.

%% @doc oneOf
%% @private
check_one_of(Value, [_ | _] = Schemas, State) ->
  check_one_of_(Value, Schemas, State, 0, []);
check_one_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_one_of_schema_array, State).

check_one_of_(_Value, [], State, 1, _Errors) ->
  State;
check_one_of_(Value, [], State, 0, Errors) ->
  handle_data_invalid({?not_one_schema_valid, Errors}, Value, State);
check_one_of_(Value, _Schemas, State, Valid, _Errors) when Valid > 1 ->
  handle_data_invalid(?more_than_one_schema_valid, Value, State);
check_one_of_(Value, [Schema | Schemas], State, Valid, Errors) ->
  ErrorsBefore = jesse_state:get_error_list(State),
  NumErrsBefore = length(ErrorsBefore),
  case validate_schema(Value, Schema, State) of
    {true, NewState} ->
      ErrorsAfter = jesse_state:get_error_list(NewState),
      case length(ErrorsAfter) of
        NumErrsBefore ->
          check_one_of_(Value, Schemas, NewState, Valid + 1, Errors);
        _  ->
          NewErrors = ErrorsAfter -- ErrorsBefore,
          check_one_of_(Value, Schemas, State, Valid, Errors ++ NewErrors)
      end;
    {false, NewErrors} ->
      check_one_of_(Value, Schemas, State, Valid, Errors ++ NewErrors)
  end.

%% @doc not
%% @private
check_not(Value, Schema, State) ->
  case validate_schema(Value, Schema, State) of
    {true, _}  -> handle_data_invalid(?not_schema_valid, Value, State);
    {false, _} -> State
  end.

%% @doc Validate a value against a schema in a given state.
%% Used by all combinators to run validation on a schema.
%% @private
validate_schema(Value, Schema0, State0) ->
  Schema = canonical(Schema0),
  try
    case jesse_lib:is_json_object(Schema) of
      true ->
        State1 = set_current_schema(State0, Schema),
        State2 = jesse_schema_validator:validate_with_state( Schema
                                                           , Value
                                                           , State1
                                                           ),
        {true, set_current_schema(State2, get_current_schema(State0))};
      false ->
        handle_schema_invalid(?schema_invalid, State0)
    end
  catch
    throw:Errors -> {false, Errors}
  end.

canonical(true) ->
  #{};
canonical(false) ->
  #{?NOT => #{}};
canonical(MaybeObject) ->
  MaybeObject.

%% @private
validate_ref(Value, Reference, State) ->
  case resolve_ref(Reference, State) of
    {error, NewState} ->
      undo_resolve_ref(NewState, State);
    {ok, NewState, Schema0} ->
      Schema = canonical(Schema0),
      ResultState =
        jesse_schema_validator:validate_with_state(Schema, Value, NewState),
      undo_resolve_ref(ResultState, State)
  end.

%% @doc Resolve a JSON reference
%% The "$id" keyword is taken care of behind the scenes in jesse_state.
%% @private
resolve_ref(Reference, State) ->
  CurrentErrors = jesse_state:get_error_list(State),
  NewState = jesse_state:resolve_ref(State, Reference),
  NewErrors = jesse_state:get_error_list(NewState),
  case length(CurrentErrors) =:= length(NewErrors) of
    true ->
      Schema = get_current_schema(NewState),
      {ok, NewState, Schema};
    false -> {error, NewState}
  end.

undo_resolve_ref(State, OriginalState) ->
  jesse_state:undo_resolve_ref(State, OriginalState).

%%=============================================================================
%% Wrappers
%% @private
get_value(Key, Schema) ->
  jesse_json_path:value(Key, Schema, ?not_found).

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%% @private
-spec handle_data_invalid( Info :: data_error_type()
                         , Value :: jesse:json_term()
                         , State :: jesse_state:state()
                         ) -> jesse_state:state().
handle_data_invalid(Info, Value, State) ->
  jesse_error:handle_data_invalid(Info, Value, State).

%% @private
-spec handle_schema_invalid( Info :: schema_error_type()
                           , State :: jesse_state:state()
                           ) -> jesse_state:state().
handle_schema_invalid(Info, State) ->
  jesse_error:handle_schema_invalid(Info, State).

%% @private
get_current_schema(State) ->
  jesse_state:get_current_schema(State).

%% @private
set_current_schema(State, NewSchema) ->
  jesse_state:set_current_schema(State, NewSchema).

%% @private
empty_if_not_found(Value) ->
  jesse_lib:empty_if_not_found(Value).

%% @private
add_to_path(State, Property) ->
  jesse_state:add_to_path(State, Property).

%% @private
remove_last_from_path(State) ->
  jesse_state:remove_last_from_path(State).

maybe_external_check_value(Value, State) ->
  case jesse_state:get_external_validator(State) of
    undefined ->
      State;
    Fun ->
      Fun(Value, State)
  end.

%% @private
-spec shortest(list() | empty, list() | empty) -> list() | empty.
shortest(X, empty) ->
  X;
shortest(empty, Y) ->
  Y;
shortest(X, Y) when length(X) < length(Y) ->
  X;
shortest(_, Y) ->
  Y.
