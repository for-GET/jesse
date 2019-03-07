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

-module(jesse_error).

-export([ default_error_handler/3
        , handle_data_invalid/3
        , handle_schema_invalid/2
        , to_json/1
        , to_json/2
        , reason_to_jsx/1
        ]).

-export_type([ error/0
             , error_reason/0
             ]).

-type error() :: {error, [error_reason()]}.

-type error_reason() :: { schema_invalid
                        , Schema :: jesse:json_term()
                        , Error  :: error_type()
                        }
                      | { data_invalid
                        , Schema :: jesse:json_term()
                        , Error  :: error_type()
                        , Data   :: jesse:json_term()
                        , Path   :: [binary()]
                        }
                      | { data_error
                        , {parse_error, _}
                        }
                      | { schema_error
                        , {parse_error, _}
                        }.

-type error_type() :: atom()
                    | {atom(), jesse:json_term()}
                    | {atom(), binary()}.

%% Includes
-include("jesse_schema_validator.hrl").

%% @doc Implements the default error handler.
%% If the length of `ErrorList' exceeds `AllowedErrors' then the function
%% throws an exeption, otherwise adds a new element to the list and returs it.
-spec default_error_handler( Error         :: error_reason()
                           , ErrorList     :: [error_reason()]
                           , AllowedErrors :: jesse_state:allowed_errors()
                           ) -> [error_reason()] | no_return().
default_error_handler(Error, ErrorList0, AllowedErrors) ->
  ErrorList = ErrorList0 ++ [Error],
  case AllowedErrors > length(ErrorList0) orelse AllowedErrors =:= ?infinity of
    true  ->
      ErrorList;
    false ->
      throw(ErrorList)
  end.

%% @doc Generates a new data error and returns the updated state.
-spec handle_data_invalid( Info  :: error_type()
                         , Value :: jesse:json_term()
                         , State :: jesse_state:state()
                         ) -> jesse_state:state().
handle_data_invalid(Info, Value, State) ->
  Error = { ?data_invalid
          , jesse_state:get_current_schema(State)
          , Info
          , Value
          , lists:reverse(jesse_state:get_current_path(State))
          },
  handle_error(Error, State).

%% @doc Generates a new schema error and returns the updated state.
-spec handle_schema_invalid( Info  :: schema_invalid | error_type()
                           , State :: jesse_state:state()
                           ) -> jesse_state:state().
handle_schema_invalid(Info, State) ->
  Error = { ?schema_invalid
          , jesse_state:get_current_schema(State)
          , Info
          },
  handle_error(Error, State).

%% @doc Convert an error to a JSON string using jsx
-spec to_json(Error :: error()) -> binary().
to_json(Error) ->
  to_json(Error, [indent]).

%% @doc Convert an error to a JSON string using jsx
-spec to_json(Error :: error(), JsxOptions :: [atom()]) -> binary().
to_json({error, Reasons}, JsxOptions) ->
  JsxReasons = lists:map(fun reason_to_jsx/1, Reasons),
  jsx:encode([{reasons, JsxReasons}], JsxOptions).

%% @doc Convert an error reason to jsx structs
-spec reason_to_jsx(Reason :: error_reason()) -> jesse:json_term().
reason_to_jsx({?schema_invalid, Schema, {Error, ErrorDetails}}) ->
  [ {invalid, schema}
  , {schema, Schema}
  , {error, Error}
  , {error_details, ErrorDetails}
  ];
reason_to_jsx({?schema_invalid, Schema, Error}) ->
  [ {invalid, schema}
  , {schema, Schema}
  , {error, Error}
  ];
reason_to_jsx({?data_invalid, Schema, {Error, ErrorDetails}, Data, Path}) ->
  [ {invalid, data}
  , {schema, Schema}
  , {error, Error}
  , {error_details, ErrorDetails}
  , {data, Data}
  , {path, Path}
  ];
reason_to_jsx({?data_invalid, Schema, Error, Data, Path}) ->
  [ {invalid, data}
  , {schema, Schema}
  , {error, Error}
  , {data, Data}
  , {path, Path}
  ].

%%% Internal functions
%% @private
handle_error(Error, State) ->
  ErrorHandler  = jesse_state:get_error_handler(State),
  ErrorList     = jesse_state:get_error_list(State),
  AllowedErrors = jesse_state:get_allowed_errors(State),
  NewErrorList  = ErrorHandler(Error, ErrorList, AllowedErrors),
  jesse_state:set_error_list(State, NewErrorList).

%% @private
