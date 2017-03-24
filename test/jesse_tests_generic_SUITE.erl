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
%% @doc jesse test suite which covers generic jessy functionality
%% (not schema specific).
%% @end
%%%=============================================================================

-module(jesse_tests_generic_SUITE).

-behaviour(jesse_schema_validator).

-compile([ export_all
         ]).

-define(EXCLUDED_FUNS, [ module_info
                       , all
                       , init_per_suite
                       , end_per_suite
                       , init_state
                       , check_value
                       , update_custom_state
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
  get_tests( "Generic-Test-Suite"
           , <<"http://json-schema.org/draft-04/schema#">>
           )
    ++ Config.

end_per_suite(_Config) ->
  inets:stop().

init_state() ->
  0.

check_value(Value, {<<"customDef">>, Property}, State0) ->
  State = update_custom_state(State0),
  case jesse_json_path:path(Property, Value) of
    true  -> State;
    false -> jesse_error:handle_data_invalid( 'custom_validator_reject'
                                            , Value
                                            , State);
    %% Skip if custom property is missing
    [] -> State
  end;
check_value(Value, Attr, State) ->
  jesse_validator_draft4:check_value(Value, Attr, State).

update_custom_state(State) ->
  ValidatorState = 0 = jesse_state:get_validator_state(State),
  jesse_state:set_validator_state(State, ValidatorState + 1).

%%% Testcases

additionalItems(Config) ->
  do_test("customValidator", Config).
