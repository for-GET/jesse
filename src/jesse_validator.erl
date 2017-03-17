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
%% This module is the behaviour definition for jesse validator.
%% @end
%%%=============================================================================

-module(jesse_validator).

%% API
-export([ init_state/2
        , check_value/4
        ]).

%% Behaviour definition
-callback init_state(Opts :: opts()) ->
  state().

-callback check_value( Value :: any()
                     , Attr  :: {binary(), jesse:json_term()}
                     , State :: jesse_state:state()
                     ) -> jesse_state:state() | no_return().


-type opts() :: any().

-type state() :: any().

-export_type([ opts/0
             , state/0
             ]).

%% API
-spec init_state( Validator :: module()
                , Opts      :: opts()
                ) -> state().
init_state(Validator, Opts) ->
  Validator:init_state(Opts).

-spec check_value( Validator :: module()
                 , Value     :: any()
                 , Attr      :: {binary(), jesse:json_term()}
                 , State     :: jesse_state:state()
                 ) -> jesse_state:state() | no_return().
check_value(Validator, Value, Attr, State) ->
  Validator:check_value(Value, Attr, State).
