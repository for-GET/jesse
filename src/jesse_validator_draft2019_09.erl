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
%%   * "unevaluatedProperties"/"unevaluatedItems": see the annotation model
%%     below.
%%
%% == unevaluated* annotation model ==
%%
%% "unevaluatedProperties"/"unevaluatedItems" apply to the object properties /
%% array items that were NOT "evaluated" by any adjacent keyword or by a
%% *successful* in-place applicator (allOf/anyOf/oneOf/if-then-else/$ref/
%% dependentSchemas). To track this, each schema-object evaluation carries an
%% "evaluated" accumulator in `jesse_state' (a `{PropNameSet, ItemIndexSet}').
%% `check_value/3' resets it on entry (so cousins in separate subschemas can't
%% see each other's annotations) and returns the set for that object; in-place
%% applicators merge the sets of their *passing* subschemas upward; child
%% instance recursion restores the parent's set. `not' never contributes
%% (its subschema must fail). This mirrors the 2019-09 annotation rules and is
%% exercised by the official test-suite cousin/uncle/nested cases.
%%
%% Safety rule: a keyword that this dialect *defines* but jesse does not yet
%% implement (`$recursiveRef') raises `keyword_not_supported' rather than being
%% silently ignored, so it can never false-accept data the keyword would have
%% rejected. Only genuinely annotation-only keywords are ignored.
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

%% The evaluated-annotation accumulator: sets of evaluated property names and
%% item indexes for the schema object currently being validated.
-type evaluated() :: {#{binary() => true}, #{non_neg_integer() => true}}.

%%% API
%% @doc Validate `Value' against the schema object `JsonSchema'.
%%
%% This is the per-schema-object entry point: it resets the evaluated
%% accumulator, walks the keywords, then applies "unevaluatedProperties"/
%% "unevaluatedItems" against whatever was left un-evaluated. On return the
%% state's evaluated set describes what this object evaluated, for the caller
%% (an in-place applicator) to merge upward.
-spec check_value( Value :: jesse:json_term()
                 , JsonSchema :: jesse:schema()
                 , State :: jesse_state:state()
                 ) -> jesse_state:state() | no_return().
check_value(Value, JsonSchema, State0) ->
  State1 = set_evaluated(State0, ev_new()),
  State2 = walk(Value, JsonSchema, State1),
  apply_unevaluated(Value, JsonSchema, State2).

%%% Internal functions
%% @doc Walk the keyword list of a single schema object, accumulating both
%% validation errors and the evaluated-annotation set.
%% @private
%% Draft 2019-09: "$ref" is evaluated alongside its sibling keywords rather than
%% replacing the schema, so we continue the keyword walk after resolving it.
walk(Value, [{?REF, RefSchemaURI} | Attrs], State) ->
  NewState = validate_ref(Value, RefSchemaURI, State),
  walk(Value, Attrs, NewState);
walk(Value, [{?TYPE, Type} | Attrs], State) ->
  NewState = check_type(Value, Type, State),
  walk(Value, Attrs, NewState);
walk(Value, [{?PROPERTIES, Properties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_properties( Value
                                        , unwrap(Properties)
                                        , State
                                        );
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk( Value
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
  walk(Value, Attrs, NewState);
walk( Value
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
  walk(Value, Attrs, NewState);
walk( Value
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
  walk(Value, Attrs, NewState);
walk(Value, [{?ITEMS, Items} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_items(Value, Items, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
%% "additionalItems" is consumed together with the tuple form of "items".
walk( Value
    , [{?ADDITIONALITEMS, _AdditionalItems} | Attrs]
    , State
    ) ->
  walk(Value, Attrs, State);
walk(Value, [{?CONTAINS, Schema} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_contains(Value, Schema, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
%% "minContains"/"maxContains" are consumed together with "contains" (which
%% reads them off the current schema). Standalone, they have no effect.
walk(Value, [{?MINCONTAINS, _} | Attrs], State) ->
  walk(Value, Attrs, State);
walk(Value, [{?MAXCONTAINS, _} | Attrs], State) ->
  walk(Value, Attrs, State);
walk(Value, [{?REQUIRED, Required} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_required(Value, Required, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?DEPENDENTREQUIRED, Dependencies} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_dependent_required(Value, Dependencies, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?DEPENDENTSCHEMAS, Dependencies} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_dependent_schemas(Value, Dependencies, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?IF, IfSchema} | Attrs], State) ->
  NewState = check_if_then_else(Value, canonical(IfSchema), State),
  walk(Value, Attrs, NewState);
%% "then"/"else" are applied by the "if" clause above; alone they are inert.
walk(Value, [{?THEN, _} | Attrs], State) ->
  walk(Value, Attrs, State);
walk(Value, [{?ELSE, _} | Attrs], State) ->
  walk(Value, Attrs, State);
walk(Value, [{?MINIMUM, Minimum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_minimum(Value, Minimum, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?EXCLUSIVEMINIMUM, ExclusiveMinimum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_exclusive_minimum(Value, ExclusiveMinimum, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MAXIMUM, Maximum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_maximum(Value, Maximum, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?EXCLUSIVEMAXIMUM, ExclusiveMaximum} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_exclusive_maximum(Value, ExclusiveMaximum, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MINITEMS, MinItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_min_items(Value, MinItems, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MAXITEMS, MaxItems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_max_items(Value, MaxItems, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?UNIQUEITEMS, Uniqueitems} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  -> check_unique_items(Value, Uniqueitems, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?PATTERN, Pattern} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_pattern(Value, Pattern, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MINLENGTH, MinLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_min_length(Value, MinLength, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MAXLENGTH, MaxLength} | Attrs], State) ->
  NewState = case is_binary(Value) of
               true  -> check_max_length(Value, MaxLength, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?ENUM, Enum} | Attrs], State) ->
  NewState = check_enum(Value, Enum, State),
  walk(Value, Attrs, NewState);
walk(Value, [{?CONST, Const} | Attrs], State) ->
  NewState = check_enum(Value, [Const], State),
  walk(Value, Attrs, NewState);
%% Draft 2019-09 "format" is annotation-only by default (the format-assertion
%% vocabulary is opt-in and not implemented here), so it never asserts.
walk(Value, [{?FORMAT, _Format} | Attrs], State) ->
  walk(Value, Attrs, State);
walk(Value, [{?MULTIPLEOF, Multiple} | Attrs], State) ->
  NewState = case is_number(Value) of
               true  -> check_multiple_of(Value, Multiple, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MAXPROPERTIES, MaxProperties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_max_properties(Value, MaxProperties, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?MINPROPERTIES, MinProperties} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_min_properties(Value, MinProperties, State);
               false -> State
             end,
  walk(Value, Attrs, NewState);
walk(Value, [{?ALLOF, Schemas} | Attrs], State) ->
  NewState = check_all_of(Value, Schemas, State),
  walk(Value, Attrs, NewState);
walk(Value, [{?ANYOF, Schemas} | Attrs], State) ->
  NewState = check_any_of(Value, Schemas, State),
  walk(Value, Attrs, NewState);
walk(Value, [{?ONEOF, Schemas} | Attrs], State) ->
  NewState = check_one_of(Value, Schemas, State),
  walk(Value, Attrs, NewState);
walk(Value, [{?NOT, Schema} | Attrs], State) ->
  NewState = check_not(Value, canonical(Schema), State),
  walk(Value, Attrs, NewState);
%% "unevaluatedProperties"/"unevaluatedItems" are deferred to apply_unevaluated,
%% which runs after every adjacent keyword and in-place applicator.
walk(Value, [{?UNEVALUATEDPROPERTIES, _} | Attrs], State) ->
  walk(Value, Attrs, State);
walk(Value, [{?UNEVALUATEDITEMS, _} | Attrs], State) ->
  walk(Value, Attrs, State);
%% Defined-but-not-yet-implemented keyword: surface an error instead of
%% silently ignoring it (which would false-accept). See milestone J3.
walk(Value, [{?RECURSIVEREF, _} | Attrs], State) ->
  NewState = unsupported_keyword(?RECURSIVEREF, State),
  walk(Value, Attrs, NewState);
walk(Value, Bool, State) when is_boolean(Bool) ->
  %% Boolean schemas: true always passes, false always fails.
  walk(Value, unwrap(canonical(Bool)), State);
walk(Value, [], State) ->
  maybe_external_check_value(Value, State);
%% Unknown keywords (including "$id", "$anchor", "$defs", "$comment",
%% "$vocabulary", "$recursiveAnchor", "definitions", the annotation/metadata
%% keywords, and any content-vocabulary keyword) carry no assertion and are
%% ignored, per spec. Identifier keywords are consumed by `jesse_state'.
walk(Value, [_Attr | Attrs], State) ->
  walk(Value, Attrs, State).

%% @doc Raise a schema error for a keyword the dialect defines but which jesse
%% does not implement yet, so it surfaces instead of false-accepting.
%% @private
unsupported_keyword(Keyword, State) ->
  handle_schema_invalid({?keyword_not_supported, Keyword}, State).

%% @doc Validate a child instance (property value / array item) against its
%% subschema. The child is a fresh schema-object evaluation with its own
%% evaluated set, so we restore the parent's evaluated set afterward — the
%% parent keyword handler is responsible for recording the child key/index.
%% @private
check_value(Property, Value, Attrs, State) ->
  ParentEvaluated = get_evaluated(State),
  State1 = add_to_path(State, Property),
  State2 = jesse_schema_validator:validate_with_state(Attrs, Value, State1),
  State3 = remove_last_from_path(State2),
  set_evaluated(State3, ParentEvaluated).

%%=============================================================================
%% Evaluated-annotation accumulator helpers
%% @private
-spec ev_new() -> evaluated().
ev_new() -> {#{}, #{}}.

%% @private
ev_add_props(State, Names) ->
  {Props, Items} = get_evaluated(State),
  Props1 = lists:foldl(fun(N, Acc) -> Acc#{N => true} end, Props, Names),
  set_evaluated(State, {Props1, Items}).

%% @private
ev_add_items(State, Indexes) ->
  {Props, Items} = get_evaluated(State),
  Items1 = lists:foldl(fun(I, Acc) -> Acc#{I => true} end, Items, Indexes),
  set_evaluated(State, {Props, Items1}).

%% @doc Merge the evaluated set of a successful in-place applicator subschema
%% (`From') into the base state. Used to propagate annotations upward.
%% @private
ev_merge(Base, From) ->
  {Pb, Ib} = get_evaluated(Base),
  {Pf, If} = get_evaluated(From),
  set_evaluated(Base, {maps:merge(Pb, Pf), maps:merge(Ib, If)}).

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


%% @doc properties. Records every present, matching property as evaluated.
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
  State1 = set_current_schema(TmpState, get_current_schema(State)),
  PresentNames = [ PN || {PN, _} <- Properties
                       , get_value(PN, Value) =/= ?not_found ],
  ev_add_props(State1, PresentNames).

%% @doc patternProperties. Records every property matching a pattern as
%% evaluated.
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
  State1 = set_current_schema(TmpState, get_current_schema(State)),
  Matched = [ PN
              || {PN, _} <- unwrap(Value)
               , {Pat, _} <- unwrap(PatternProperties)
               , jesse_lib:re_run(PN, Pat) =:= match ],
  ev_add_props(State1, Matched).

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

%% @doc additionalProperties. Records the "additional" properties (those not
%% covered by properties/patternProperties) as evaluated when the keyword
%% permits them.
%% @private
check_additional_properties(Value, false, State) ->
  case additional_property_names(Value, State) of
    []     -> State;
    Extras ->
      lists:foldl( fun(Property, State1) ->
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
check_additional_properties(Value, true, State) ->
  ev_add_props(State, additional_property_names(Value, State));
check_additional_properties(Value, AdditionalProperties, State) ->
  JsonSchema = get_current_schema(State),
  case additional_property_names(Value, State) of
    []     -> State;
    Extras ->
      TmpState
        = lists:foldl( fun(ExtraName, CurrentState) ->
                           NewState = set_current_schema( CurrentState
                                                        , AdditionalProperties
                                                        ),
                           check_value( ExtraName
                                      , get_value(ExtraName, Value)
                                      , AdditionalProperties
                                      , NewState
                                      )
                       end
                     , State
                     , Extras
                     ),
      State1 = set_current_schema(TmpState, JsonSchema),
      ev_add_props(State1, Extras)
  end.

%% @doc Names of the properties not covered by "properties" or
%% "patternProperties" of the current schema.
%% @private
additional_property_names(Value, State) ->
  JsonSchema        = get_current_schema(State),
  Properties        = empty_if_not_found(get_value(?PROPERTIES, JsonSchema)),
  PatternProperties = empty_if_not_found(get_value( ?PATTERNPROPERTIES
                                                  , JsonSchema)),
  ValuePropertiesNames  = [Name || {Name, _} <- unwrap(Value)],
  SchemaPropertiesNames = [Name || {Name, _} <- unwrap(Properties)],
  Patterns    = [Pattern || {Pattern, _} <- unwrap(PatternProperties)],
  ExtraNames0 = lists:subtract(ValuePropertiesNames, SchemaPropertiesNames),
  lists:foldl( fun(Pattern, ExtraAcc) ->
                   filter_extra_names(Pattern, ExtraAcc)
               end
             , ExtraNames0
             , Patterns
             ).

%% @private
filter_extra_names(Pattern, ExtraNames) ->
  Filter = fun(ExtraName) ->
               case jesse_lib:re_run(ExtraName, Pattern) of
                 match   -> false;
                 nomatch -> true
               end
           end,
  lists:filter(Filter, ExtraNames).

%% @doc items / additionalItems. Records the covered indexes as evaluated.
%% @private
check_items(Value, Items0, State) ->
  case jesse_lib:is_json_object(Items0) orelse is_boolean(Items0) of
    true ->
      %% Single-schema form: applies to (and evaluates) every item.
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
      State1 = set_current_schema(TmpState, get_current_schema(State)),
      ev_add_items(State1, lists:seq(0, length(Value) - 1));
    false when is_list(Items0) ->
      check_items_array(Value, lists:map(fun canonical/1, Items0), State);
    _ ->
      handle_schema_invalid({?wrong_type_items, Items0}, State)
  end.

%% @doc contains / minContains / maxContains
%%
%% An array is valid if the number of elements matching the "contains" schema is
%% at least "minContains" (default 1) and at most "maxContains" (default
%% unbounded). "minContains" of 0 makes "contains" trivially satisfied. Matching
%% items are recorded as evaluated.
%% @private
check_contains(Values, Schema0, State) ->
  Schema     = canonical(Schema0),
  JsonSchema = get_current_schema(State),
  MinContains = contains_bound(get_value(?MINCONTAINS, JsonSchema), 1),
  MaxContains = contains_bound(get_value(?MAXCONTAINS, JsonSchema), ?infinity),
  MatchedIndexes = contains_matches(Values, Schema, State),
  MatchCount = length(MatchedIndexes),
  case in_contains_range(MatchCount, MinContains, MaxContains) of
    true  -> ev_add_items(State, MatchedIndexes);
    false -> handle_data_invalid(?data_invalid, Values, State)
  end.

%% @private
contains_bound(?not_found, Default) -> Default;
contains_bound(Value, _Default)     -> Value.

%% @private
in_contains_range(Count, Min, Max) ->
  Count >= Min andalso (Max =:= ?infinity orelse Count =< Max).

%% @doc Returns the indexes of the array elements matching the schema.
%% @private
contains_matches(Values, Schema, State) ->
  {_, Matched} =
    lists:foldl( fun(Value, {Index, Acc}) ->
                     case validate_schema(Value, Schema, State) of
                       {true, _}  -> {Index + 1, [Index | Acc]};
                       {false, _} -> {Index + 1, Acc}
                     end
                 end
               , {0, []}
               , Values
               ),
  lists:reverse(Matched).

%% @private
check_items_array(Value, Items, State) ->
  JsonSchema = get_current_schema(State),
  NExtra = length(Value) - length(Items),
  TupleCount = min(length(Value), length(Items)),
  case NExtra > 0 of
    true ->
      case get_value(?ADDITIONALITEMS, JsonSchema) of
        ?not_found ->
          %% Only the tuple positions are evaluated; the rest are left for
          %% "unevaluatedItems".
          State1 = check_items_fun(lists:zip( lists:sublist(Value, TupleCount)
                                            , Items)
                                  , State),
          ev_add_items(State1, lists:seq(0, TupleCount - 1));
        true ->
          State1 = check_items_fun(lists:zip( lists:sublist(Value, TupleCount)
                                            , Items)
                                  , State),
          ev_add_items(State1, lists:seq(0, length(Value) - 1));
        false ->
          handle_data_invalid(?no_extra_items_allowed, Value, State);
        AdditionalItems ->
          ExtraSchemas = lists:duplicate(NExtra, AdditionalItems),
          Tuples = lists:zip(Value, lists:append(Items, ExtraSchemas)),
          State1 = check_items_fun(Tuples, State),
          ev_add_items(State1, lists:seq(0, length(Value) - 1))
      end;
    false ->
      RelevantItems = case NExtra of
                        0 -> Items;
                        _ -> lists:sublist(Items, length(Value))
                      end,
      State1 = check_items_fun(lists:zip(Value, RelevantItems), State),
      ev_add_items(State1, lists:seq(0, length(Value) - 1))
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
%% instance must validate against when the key property is present. Each
%% dependent schema is an in-place applicator, so its evaluated set merges up.
%% @private
check_dependent_schemas(Value, Dependencies, State) ->
  lists:foldl( fun({DependencyName, DependencySchema}, CurrentState) ->
                   case get_value(DependencyName, Value) of
                     ?not_found -> CurrentState;
                     _          -> apply_dependent_schema(
                                     Value
                                    , canonical(DependencySchema)
                                    , CurrentState
                                    )
                   end
               end
             , State
             , unwrap(Dependencies)
             ).

%% @private
apply_dependent_schema(Value, DependencySchema, State) ->
  case jesse_lib:is_json_object(DependencySchema) of
    true ->
      case validate_schema(Value, DependencySchema, State) of
        {true, SubState} ->
          ev_merge(State, SubState);
        {false, Errors} ->
          handle_data_invalid({?all_schemas_not_valid, Errors}, Value, State)
      end;
    false ->
      handle_schema_invalid({?wrong_type_dependency, DependencySchema}, State)
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
%% "if" itself never produces validation errors of its own, but when it passes
%% its annotations (and the branch's) are merged in.
%% @private
check_if_then_else(Value, IfSchema, State) ->
  JsonSchema = get_current_schema(State),
  case validate_schema(Value, IfSchema, State) of
    {true, IfSub} ->
      State1 = ev_merge(State, IfSub),
      apply_branch(Value, ?THEN, JsonSchema, State1);
    {false, _} ->
      apply_branch(Value, ?ELSE, JsonSchema, State)
  end.

%% @private
apply_branch(Value, Keyword, JsonSchema, State) ->
  case get_value(Keyword, JsonSchema) of
    ?not_found ->
      State;
    BranchSchema ->
      case validate_schema(Value, canonical(BranchSchema), State) of
        {true, SubState} ->
          ev_merge(State, SubState);
        {false, Errors} ->
          handle_data_invalid({?not_schema_valid, Errors}, Value, State)
      end
  end.

%% @doc minimum / exclusiveMinimum
%% @private
check_minimum(Value, Minimum, State) ->
  case (Value >= Minimum) of
    true  -> State;
    false -> handle_data_invalid(?not_in_range, Value, State)
  end.

check_exclusive_minimum(Value, ExclusiveMinimum, State) ->
  case (Value > ExclusiveMinimum) of
    true  -> State;
    false -> handle_data_invalid(?not_in_range, Value, State)
  end.

%% @doc maximum / exclusiveMaximum
%% @private
check_maximum(Value, Maximum, State) ->
  case (Value =< Maximum) of
    true  -> State;
    false -> handle_data_invalid(?not_in_range, Value, State)
  end.

check_exclusive_maximum(Value, ExclusiveMaximum, State) ->
  case (Value < ExclusiveMaximum) of
    true  -> State;
    false -> handle_data_invalid(?not_in_range, Value, State)
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
    nomatch -> handle_data_invalid(?no_match, Value, State)
  end.

%% @doc minLength
%% @private
check_min_length(Value, MinLength, State) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> State;
    false -> handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc maxLength
%% @private
check_max_length(Value, MaxLength, State) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> State;
    false -> handle_data_invalid(?wrong_length, Value, State)
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
    false -> handle_data_invalid(?not_in_enum, Value, State)
  end.

%% @doc multipleOf
%% @private
check_multiple_of(Value, MultipleOf, State)
  when is_number(MultipleOf), MultipleOf > 0 ->
  try (Value / MultipleOf - trunc(Value / MultipleOf)) * MultipleOf == 0.0 of
    true -> State;
    _    -> handle_data_invalid(?not_multiple_of, Value, State)
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

%% @doc allOf. Every subschema must pass; the union of their evaluated sets is
%% propagated up.
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
      check_all_of_(Value, Schemas, ev_merge(NewState, State));
    {false, Errors} ->
      handle_data_invalid({?all_schemas_not_valid, Errors}, Value, State)
  end.

%% @doc anyOf. Valid if at least one subschema passes; annotations from *all*
%% passing subschemas are merged in (required for "unevaluatedProperties with
%% anyOf" where several branches match).
%% @private
check_any_of(Value, [_ | _] = Schemas, State) ->
  {AnyValid, MergedState, ShortestErrors} =
    lists:foldl( fun(Schema, {Valid, StateAcc, Errors}) ->
                     case validate_schema(Value, Schema, State) of
                       {true, SubState} ->
                         {true, ev_merge(StateAcc, SubState), Errors};
                       {false, NewErrors} ->
                         {Valid, StateAcc, shortest(NewErrors, Errors)}
                     end
                 end
               , {false, State, empty}
               , Schemas
               ),
  case AnyValid of
    true  -> MergedState;
    false -> any_of_error(Value, State, ShortestErrors)
  end;
check_any_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_any_of_schema_array, State).

%% @private
any_of_error(Value, State, empty) ->
  handle_data_invalid(?any_schemas_not_valid, Value, State);
any_of_error(Value, State, Errors) ->
  handle_data_invalid({?any_schemas_not_valid, Errors}, Value, State).

%% @doc oneOf. Valid if exactly one subschema passes; that subschema's
%% evaluated set is merged in.
%% @private
check_one_of(Value, [_ | _] = Schemas, State) ->
  {ValidCount, ValidSubState, Errors} =
    lists:foldl( fun(Schema, {Count, SubAcc, ErrAcc}) ->
                     case validate_schema(Value, Schema, State) of
                       {true, SubState} ->
                         {Count + 1, SubState, ErrAcc};
                       {false, NewErrors} ->
                         {Count, SubAcc, ErrAcc ++ NewErrors}
                     end
                 end
               , {0, undefined, []}
               , Schemas
               ),
  case ValidCount of
    1 -> ev_merge(State, ValidSubState);
    0 -> handle_data_invalid({?not_one_schema_valid, Errors}, Value, State);
    _ -> handle_data_invalid(?more_than_one_schema_valid, Value, State)
  end;
check_one_of(_Value, _InvalidSchemas, State) ->
  handle_schema_invalid(?wrong_one_of_schema_array, State).

%% @doc not. The subschema must fail; "not" never contributes annotations.
%% @private
check_not(Value, Schema, State) ->
  case validate_schema(Value, Schema, State) of
    {true, _}  -> handle_data_invalid(?not_schema_valid, Value, State);
    {false, _} -> State
  end.

%% @doc unevaluatedProperties / unevaluatedItems. Applied after every adjacent
%% keyword and in-place applicator has contributed to the evaluated set.
%% @private
apply_unevaluated(Value, Schema, State) when is_list(Schema) ->
  State1 = apply_unevaluated_properties(Value, Schema, State),
  apply_unevaluated_items(Value, Schema, State1);
apply_unevaluated(_Value, _Schema, State) ->
  State.

%% @private
apply_unevaluated_properties(Value, Schema, State) ->
  case schema_keyword(?UNEVALUATEDPROPERTIES, Schema) of
    ?not_found ->
      State;
    UnevalSchema ->
      case jesse_lib:is_json_object(Value) of
        true  ->
          check_unevaluated_properties(Value, canonical(UnevalSchema), State);
        false ->
          State
      end
  end.

%% @private
check_unevaluated_properties(Value, UnevalSchema, State) ->
  {EvaluatedProps, _} = get_evaluated(State),
  Leftover = [ {N, V} || {N, V} <- unwrap(Value)
                       , not maps:is_key(N, EvaluatedProps) ],
  case Leftover of
    [] ->
      State;
    _ ->
      TmpState =
        lists:foldl( fun({N, V}, CurrentState) ->
                         NewState = set_current_schema( CurrentState
                                                      , UnevalSchema),
                         check_value(N, V, UnevalSchema, NewState)
                     end
                   , State
                   , Leftover
                   ),
      State1 = set_current_schema(TmpState, get_current_schema(State)),
      %% The leftovers are now evaluated; record them so that an enclosing
      %% "unevaluatedProperties" (via an in-place applicator) sees them.
      ev_add_props(State1, [N || {N, _} <- Leftover])
  end.

%% @private
apply_unevaluated_items(Value, Schema, State) ->
  case schema_keyword(?UNEVALUATEDITEMS, Schema) of
    ?not_found ->
      State;
    UnevalSchema ->
      case jesse_lib:is_array(Value) of
        true  ->
          check_unevaluated_items(Value, canonical(UnevalSchema), State);
        false ->
          State
      end
  end.

%% @private
check_unevaluated_items(Value, UnevalSchema, State) ->
  {_, EvaluatedItems} = get_evaluated(State),
  Indexed  = lists:zip(lists:seq(0, length(Value) - 1), Value),
  Leftover = [ {I, V} || {I, V} <- Indexed
                       , not maps:is_key(I, EvaluatedItems) ],
  case Leftover of
    [] ->
      State;
    _ ->
      TmpState =
        lists:foldl( fun({I, V}, CurrentState) ->
                         NewState = set_current_schema( CurrentState
                                                      , UnevalSchema),
                         check_value(I, V, UnevalSchema, NewState)
                     end
                   , State
                   , Leftover
                   ),
      State1 = set_current_schema(TmpState, get_current_schema(State)),
      ev_add_items(State1, [I || {I, _} <- Leftover])
  end.

%% @doc Validate a value against a schema, returning the resulting state (which
%% carries the subschema's evaluated set) on success. Used by all in-place
%% applicators to run and roll back a subschema evaluation.
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
  ParentEvaluated = get_evaluated(State),
  ResultState =
    case resolve_ref(Reference, State) of
      {error, NewState} ->
        undo_resolve_ref(NewState, State);
      {ok, NewState, Schema0} ->
        Schema = canonical(Schema0),
        RefState =
          jesse_schema_validator:validate_with_state(Schema, Value, NewState),
        undo_resolve_ref(RefState, State)
    end,
  %% "$ref" is an in-place applicator: merge the ref target's evaluated set
  %% into the referring schema's.
  {RefProps, RefItems} = get_evaluated(ResultState),
  {PProps, PItems} = ParentEvaluated,
  set_evaluated( ResultState
               , {maps:merge(PProps, RefProps), maps:merge(PItems, RefItems)}).

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

%% @doc Look up a schema keyword by exact key, returning `?not_found' when
%% absent. Unlike `get_value/2' this uses a plain proplist lookup and so is
%% safe on the empty object (which `jesse_json_path' otherwise treats as a
%% KVC list and returns `[]' for any key).
%% @private
schema_keyword(Key, Schema) ->
  case lists:keyfind(Key, 1, unwrap(Schema)) of
    {_, Value} -> Value;
    false      -> ?not_found
  end.

%% @private
unwrap(Value) ->
  jesse_json_path:unwrap_value(Value).

%% @private
get_evaluated(State) ->
  jesse_state:get_evaluated(State).

%% @private
set_evaluated(State, Evaluated) ->
  jesse_state:set_evaluated(State, Evaluated).

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
