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

-module(jesse_schema_validator).

%% API
-export([ validate/3
        , validate_with_state/3
        ]).

%% Includes
-include("jesse_schema_validator.hrl").

%% Behaviour definition
-callback check_value(Value, Attr, State) ->
  State | no_return()
    when
    Value :: any(),
    Attr  :: {binary(), jesse:json_term()},
    State :: jesse_state:state().

-callback init_state() -> any() | undefined.

%%% API
%% @doc Validates json `Data' against `JsonSchema' with `Options'.
%% If the given json is valid, then it is returned to the caller as is,
%% otherwise an exception will be thrown.
-spec validate( JsonSchema :: jesse:json_term()
              , Data       :: jesse:json_term()
              , Options    :: [{Key :: atom(), Data :: any()}]
              ) -> {ok, jesse:json_term()}
                 | no_return().
validate(JsonSchema, Value, Options) ->
  State    = jesse_state:new(JsonSchema, Options),
  NewState = validate_with_state(JsonSchema, Value, State),
  {result(NewState), Value}.

%% @doc Validates json `Data' against `JsonSchema' with `State'.
%% If the given json is valid, then the latest state is returned to the caller,
%% otherwise an exception will be thrown.
-spec validate_with_state( JsonSchema :: jesse:json_term()
                         , Data       :: jesse:json_term()
                         , State      :: jesse_state:state()
                         ) -> jesse_state:state()
                            | no_return().
validate_with_state(JsonSchema0, Value, State) ->
  Validator = select_validator(JsonSchema0, State),
  JsonSchema = jesse_json_path:unwrap_value(JsonSchema0),
  run_validator(Validator, Value, JsonSchema, State).


%%% Internal functions
%% @doc Gets validator from the state or else
%% selects an appropriate one by schema version.
%% @private
select_validator(JsonSchema, State) ->
  case jesse_state:get_validator(State) of
    undefined ->
      select_validator_by_schema(get_schema_ver(JsonSchema, State), State);
    Validator ->
      Validator
  end.

select_validator_by_schema(?json_schema_draft3, _) ->
  jesse_validator_draft3;
select_validator_by_schema(?json_schema_draft4, _) ->
  jesse_validator_draft4;
select_validator_by_schema(SchemaURI, State) ->
  jesse_error:handle_schema_invalid({?schema_unsupported, SchemaURI}, State).

%% @doc Returns "$schema" property from `JsonSchema' if it is present,
%% otherwise the default schema version from `State' is returned.
%% @private
get_schema_ver(JsonSchema, State) ->
  case jesse_json_path:value(?SCHEMA, JsonSchema, ?not_found) of
    ?not_found -> jesse_state:get_default_schema_ver(State);
    SchemaVer  -> SchemaVer
  end.

%% @doc Returns a result depending on `State'.
%% @private
result(State) ->
  ErrorList = jesse_state:get_error_list(State),
  case ErrorList of
    [] -> ok;
    _  -> throw(ErrorList)
  end.

%% @doc Goes through attributes of the given `JsonSchema' and
%% validates the `Value' against them calling `Validator'.
%% @private
run_validator(_Validator, _Value, [], State) ->
  State;
run_validator(Validator, Value, [Attr | Attrs], State0) ->
  State = Validator:check_value( Value
                               , Attr
                               , State0
                               ),
  run_validator(Validator, Value, Attrs, State).
