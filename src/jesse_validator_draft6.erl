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
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard(draft6).
%% https://datatracker.ietf.org/doc/html/draft-wright-json-schema-00
%% https://datatracker.ietf.org/doc/html/draft-wright-json-schema-validation-01
%% @end
%%%=============================================================================

-module(jesse_validator_draft6).

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
                      | ?wrong_draft6_id_tag.

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
check_value(_, [{?ID_OLD, _} | _], State) ->
  handle_schema_invalid(?wrong_draft6_id_tag, State);
check_value(Value, [{?REF, RefSchemaURI} | _], State) ->
  validate_ref(Value, RefSchemaURI, State);
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
check_value(Value, [{?EXAMPLES, _Examples} | Attrs], State) ->
  NewState = case jesse_lib:is_array(Value) of
               true  ->
                 %% No need to check.
                 %% The schema is valid, by definition, at this point.
                 State;
               false -> handle_data_invalid(?not_array, Value, State)
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?REQUIRED, Required} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_required(Value, Required, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
check_value(Value, [{?DEPENDENCIES, Dependencies} | Attrs], State) ->
  NewState = case jesse_lib:is_json_object(Value) of
               true  -> check_dependencies(Value, Dependencies, State);
               false -> State
             end,
  check_value(Value, Attrs, NewState);
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
check_value(Value, [{?FORMAT, Format} | Attrs], State) ->
  NewState = check_format(Value, Format, State),
  check_value(Value, Attrs, NewState);
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
check_value(Value, Bool, State) when is_boolean(Bool) ->
  %% 4.4.  JSON Schema documents
  %% <..>
  %% Boolean values are equivalent to the following behaviors:
  %% true  Always passes validation, as if the empty schema {}
  %% false  Always fails validation, as if the schema { "not":{} }
  check_value(Value, unwrap(canonical(Bool)), State);
check_value(Value, [], State) ->
  maybe_external_check_value(Value, State);
check_value(Value, [_Attr | Attrs], State) ->
  check_value(Value, Attrs, State).

%%% Internal functions
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
%%
%% 5.5.2.1. Valid values
%%
%%   The value of this keyword MUST be either a string or an array. If it is an
%%   array, elements of the array MUST be strings and MUST be unique.
%%
%%   String values MUST be one of the seven primitive types defined by the core
%%   specification.
%%
%%  5.5.2.2. Conditions for successful validation
%%    An instance matches successfully if its primitive type is one of the types
%%    defined by keyword. Recall: "number" includes "integer".
%%
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
%%
%% Draft 6
%%
%% In draft-04"integer" is listed as a primitive type and defined as
%% “a JSON number without a fraction or exponent part”; in draft-06, "integer"
%% is not considered a primitive type and is only defined in the section for
%% keyword "type" as “any number with a zero fractional part”; 1.0 is thus not a
%% valid "integer" type in draft-04 and earlier, but is a valid "integer" type
%% in draft-06 and later; note that both drafts say that integers SHOULD be
%% encoded in JSON without fractional parts
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


%% @doc 6.20.  additionalProperties
%%
%% The value of "additionalProperties" MUST be a valid JSON Schema.
%%
%% This keyword determines how child instances validate for objects, and
%% does not directly validate the immediate instance itself.
%%
%% Validation with "additionalProperties" applies only to the child
%% values of instance names that do not match any names in "properties",
%% and do not match any regular expression in "patternProperties".
%%
%% For all such properties, validation succeeds if the child instance
%% validates against the "additionalProperties" schema.
%%
%% Omitting this keyword has the same behavior as an empty schema.
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
%% See check_properties/3.
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
%% See check_properties/3.
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

%% @doc 6.10. additionalItems and items
%%
%% The value of "additionalItems" MUST be a valid JSON Schema.
%%
%% This keyword determines how child instances validate for arrays, and
%% does not directly validate the immediate instance itself.
%%
%% If "items" is an array of schemas, validation succeeds if every
%% instance element at a position greater than the size of "items"
%% validates against "additionalItems".
%%
%% Otherwise, "additionalItems" MUST be ignored, as the "items" schema
%% (possibly the default value of an empty schema) is applied to all
%% elements.
%%
%% Omitting this keyword has the same behavior as an empty schema.
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

check_contains([], _Schema, State) ->
  handle_data_invalid(?data_invalid, [], State);
check_contains(Values, Schema0, State) ->
  Schema = canonical(Schema0),
  DefaultAssumption = {false, State},
  Result = lists:foldl(fun (Value, Acc) ->
                         case Acc of
                           {false, _} ->
                             validate_schema(Value, Schema, State);
                           {true, _} ->
                             Acc
                         end
                       end, DefaultAssumption, Values),
  case Result of
    {true, _} ->
      State;
    {false, _} ->
      handle_data_invalid(?data_invalid, Values, State)
  end.


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

%% @doc 6.21.  dependencies
%%
%%   This keyword specifies rules that are evaluated if the instance is an
%%   object and contains a certain property.
%%
%%   This keyword's value MUST be an object.  Each property specifies a
%%   dependency.  Each dependency value MUST be an array or a valid JSON
%%   Schema.
%%
%%   If the dependency value is a subschema, and the dependency key is a
%%   property in the instance, the entire instance must validate against
%%   the dependency value.
%%
%%   If the dependency value is an array, each element in the array, if
%%   any, MUST be a string, and MUST be unique.  If the dependency key is
%%   a property in the instance, each of the items in the dependency value
%%   must be a property that exists in the instance.
%%
%%   Omitting this keyword has the same behavior as an empty object.
%%
%% @private
check_dependencies(Value, Dependencies, State) ->
  lists:foldl( fun({DependencyName, DependencyValue}, CurrentState) ->
                   case get_value(DependencyName, Value) of
                     ?not_found -> CurrentState;
                     _          -> check_dependency_value(
                                     Value
                                    , DependencyName
                                    , canonical(DependencyValue)
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
    false when is_list(Dependency) ->
      check_dependency_array(Value, DependencyName, Dependency, State);
    _ ->
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
check_dependency_array(Value, DependencyName, Dependency, State) ->
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
             ).
%%
%% 6.4.  minimum
%%
%%    The value of "minimum" MUST be a number, representing an inclusive
%%    upper limit for a numeric instance.
%%
%%    If the instance is a number, then this keyword validates only if the
%%    instance is greater than or exactly equal to "minimum".
%%
%% 6.5.  exclusiveMinimum
%%
%%    The value of "exclusiveMinimum" MUST be number, representing an
%%    exclusive upper limit for a numeric instance.
%%
%%    If the instance is a number, then the instance is valid only if it
%%    has a value strictly greater than (not equal to) "exclusiveMinimum".
%%
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


%% @doc 6.2. maximum and exclusiveMaximum
%%
%% The value of "maximum" MUST be a number, representing an inclusive
%% upper limit for a numeric instance.
%%
%% If the instance is a number, then this keyword validates only if the
%% instance is less than or exactly equal to "maximum".
%%
%% 6.3.  exclusiveMaximum
%%
%% The value of "exclusiveMaximum" MUST be number, representing an
%% exclusive upper limit for a numeric instance.
%%
%% If the instance is a number, then the instance is valid only if it
%% has a value strictly less than (not equal to) "exclusiveMaximum".
%%
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

%% @doc 6.12.  minItems
%%
%% The value of this keyword MUST be a non-negative integer.
%%
%% An array instance is valid against "minItems" if its size is greater
%% than, or equal to, the value of this keyword.
%%
%% Omitting this keyword has the same behavior as a value of 0.
%%
%% @private
check_min_items(Value, MinItems, State) when length(Value) >= MinItems ->
  State;
check_min_items(Value, _MinItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc 6.11. maxItems
%%
%% The value of this keyword MUST be a non-negative integer.
%%
%% An array instance is valid against "maxItems" if its size is less
%% than, or equal to, the value of this keyword.
%%
%% @private
check_max_items(Value, MaxItems, State) when length(Value) =< MaxItems ->
  State;
check_max_items(Value, _MaxItems, State) ->
  handle_data_invalid(?wrong_size, Value, State).

%% @doc 6.13. uniqueItems
%%
%% The value of this keyword MUST be a boolean.
%%
%% If this keyword has boolean value false, the instance validates
%% successfully.  If it has boolean value true, the instance validates
%% successfully if all of its elements are unique.
%%
%% Omitting this keyword has the same behavior as a value of false.
%%
%% @private
check_unique_items(_, false, State) ->
  State;
check_unique_items([], true, State) ->
  State;
check_unique_items([_], true, State) ->
  State;
check_unique_items(Value, true, State) ->
  try
%% First we do an efficient check for duplicates: convert the list to a set
%% and if there are no duplicates, the set and the list have the same length
%% In order to avoid differences for lists in which order is not relevant
%% (e.g. JSON properties of an object maybe represented as a proplist), these
%% lists for which order is not relevant are sorted (objects are normalized).
%% If the first efficient check fails, then we search for the items that are
%% duplicated with a less efficient check (that will very seldom be executed).
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

%% @doc 6.8. pattern
%%
%% The value of this keyword MUST be a string.  This string SHOULD be a
%% valid regular expression, according to the ECMA 262 regular
%% expression dialect.
%%
%% A string instance is considered valid if the regular expression
%% matches the instance successfully.  Recall: regular expressions are
%% not implicitly anchored.
%%
%% @private
check_pattern(Value, Pattern, State) ->
  case jesse_lib:re_run(Value, Pattern) of
    match   -> State;
    nomatch ->
      handle_data_invalid(?no_match, Value, State)
  end.

%% @doc 6.7.  minLength
%%
%% The value of this keyword MUST be a non-negative integer.
%%
%% A string instance is valid against this keyword if its length is
%% greater than, or equal to, the value of this keyword.
%%
%% The length of a string instance is defined as the number of its
%% characters as defined by RFC 7159 [RFC7159].
%%
%% Omitting this keyword has the same behavior as a value of 0.
%%
%% @private
check_min_length(Value, MinLength, State) ->
  case length(unicode:characters_to_list(Value)) >= MinLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc 6.6. maxLengthmaxLength
%%
%% The value of this keyword MUST be a non-negative integer.
%%
%% A string instance is valid against this keyword if its length is less
%% than, or equal to, the value of this keyword.
%%
%% The length of a string instance is defined as the number of its
%% characters as defined by RFC 7159 [RFC7159]
%%
%% @private
check_max_length(Value, MaxLength, State) ->
  case length(unicode:characters_to_list(Value)) =< MaxLength of
    true  -> State;
    false ->
      handle_data_invalid(?wrong_length, Value, State)
  end.

%% @doc 6.23. enum
%%
%% The value of this keyword MUST be an array.  This array SHOULD have
%% at least one element.  Elements in the array SHOULD be unique.
%%
%% An instance validates successfully against this keyword if its value
%% is equal to one of the elements in this keyword's array value.
%%
%% Elements in the array might be of any value, including null.
%%
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

%% @doc format
%% Used for semantic validation.
%% @private
check_format(Value, _Format = <<"date-time">>, State) when is_binary(Value) ->
  case valid_datetime(Value) of
    true  -> State;
    false -> handle_data_invalid(?wrong_format, Value, State)
  end;
check_format(Value, _Format = <<"email">>, State) when is_binary(Value) ->
  case jesse_lib:re_run(Value, <<"^[^@]+@[^@]+$">>) of
    match   -> State;
    nomatch -> handle_data_invalid(?wrong_format, Value, State)
  end;
check_format(Value, _Format = <<"hostname">>, State) when is_binary(Value) ->
  %% not yet supported
  State;
check_format(Value, _Format = <<"ipv4">>, State) when is_binary(Value) ->
  %% avoiding inet:parse_ipv4strict_address to maintain R15 compatibility
  case inet_parse:ipv4strict_address(binary_to_list(Value)) of
    {ok, _IPv4Address} -> State;
    {error, einval}    -> handle_data_invalid(?wrong_format, Value, State)
  end;
check_format(Value, _Format = <<"ipv6">>, State) when is_binary(Value) ->
  %% avoiding inet:parse_ipv6strict_address to maintain R15 compatibility
  case inet_parse:ipv6strict_address(binary_to_list(Value)) of
    {ok, _IPv6Address} -> State;
    {error, einval}    -> handle_data_invalid(?wrong_format, Value, State)
  end;
check_format(Value, _Format = <<"uri">>, State) when is_binary(Value) ->
  %% not yet supported
  State;
check_format(Value, <<"uri-reference">>, State) when is_binary(Value) ->
  uri_reference(Value, State);
check_format(_Value, _Format, State) ->
  State.

-ifdef(OTP_RELEASE).
uri_reference(Value, State) when is_binary(Value) ->
  case uri_string:parse(Value) of
    {error, _ErrorType, _Term} ->
      handle_data_invalid(?wrong_format, Value, State);
    _ -> State
  end.
-else.
uri_reference(_Value, State) ->
  State.
-endif.


%% @doc 6.1. multipleOf
%%
%% The value of "multipleOf" MUST be a number, strictly greater than 0.
%%
%% A numeric instance is valid only if division by this keyword's value
%% results in an integer.
%%
%% @private
check_multiple_of(Value, MultipleOf, State)
  when is_number(MultipleOf), MultipleOf > 0 ->
  try (Value / MultipleOf - trunc(Value / MultipleOf)) * MultipleOf of
    Num when Num == 0.0 ->
      State;
    _   ->
      handle_data_invalid(?not_multiple_of, Value, State)
  catch error:badarith ->
      %% eg, division by zero or overflow
      handle_schema_invalid(?wrong_multiple_of, State)
  end;
check_multiple_of(_Value, _MultipleOf, State) ->
  handle_schema_invalid(?wrong_multiple_of, State).

%% @doc 6.17. required
%%
%% The value of this keyword MUST be an array.  Elements of this array,
%% if any, MUST be strings, and MUST be unique.
%%
%% An object instance is valid against this keyword if every item in the
%% array is the name of a property in the instance.
%%
%% Omitting this keyword has the same behavior as an empty array.
%%
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

%% @doc 6.15. maxProperties
%%
%% The value of this keyword MUST be a non-negative integer.
%%
%% An object instance is valid against "maxProperties" if its number of
%% properties is less than, or equal to, the value of this keyword.
%%
%% @private
check_max_properties(Value, MaxProperties, State)
  when is_integer(MaxProperties), MaxProperties >= 0 ->
    case length(unwrap(Value)) =< MaxProperties of
      true  -> State;
      false -> handle_data_invalid(?too_many_properties, Value, State)
    end;
check_max_properties(_Value, _MaxProperties, State) ->
  handle_schema_invalid(?wrong_max_properties, State).

%% @doc 6.16. minProperties
%%
%% The value of this keyword MUST be a non-negative integer.
%%
%% An object instance is valid against "minProperties" if its number of
%% properties is greater than, or equal to, the value of this keyword.
%%
%% Omitting this keyword has the same behavior as a value of 0.
%%
%% @private
check_min_properties(Value, MinProperties, State)
  when is_integer(MinProperties), MinProperties >= 0 ->
    case length(unwrap(Value)) >= MinProperties of
      true  -> State;
      false -> handle_data_invalid(?too_few_properties, Value, State)
    end;
check_min_properties(_Value, _MaxProperties, State) ->
  handle_schema_invalid(?wrong_min_properties, State).

%% @doc 6.26. allOf
%%
%% This keyword's value MUST be a non-empty array.  Each item of the
%% array MUST be a valid JSON Schema.
%%
%% An instance validates successfully against this keyword if it
%% validates successfully against all schemas defined by this keyword's
%% value.
%%
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

%% @doc 6.27. anyOf
%%
%% This keyword's value MUST be a non-empty array.  Each item of the
%% array MUST be a valid JSON Schema.
%%
%% An instance validates successfully against this keyword if it
%% validates successfully against at least one schema defined by this
%% keyword's value.
%%
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

%% @doc 6.28. oneOf
%%
%% This keyword's value MUST be a non-empty array.  Each item of the
%% array MUST be a valid JSON Schema.
%%
%% An instance validates successfully against this keyword if it
%% validates successfully against exactly one schema defined by this
%% keyword's value.
%%
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

%% @doc 6.29. not
%%
%% This keyword's value MUST be a valid JSON Schema.
%%
%% An instance is valid against this keyword if it fails to validate
%% successfully against the schema defined by this keyword.
%%
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
%% The "id" keyword is taken care of behind the scenes in jesse_state.
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

%% @private
valid_datetime(DateTimeBin) ->
  case rfc3339:parse(DateTimeBin) of
    {ok, _} ->
      true;
    _ ->
      false
  end.

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
