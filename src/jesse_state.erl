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

-module(jesse_state).

%% API
-export([ add_to_path/2
        , get_allowed_errors/1
        , get_current_path/1
        , get_current_schema/1
        , get_current_schema_id/1
        , get_default_schema_ver/1
        , get_validator/1
        , get_validator_state/1
        , get_error_handler/1
        , get_error_list/1
        , new/2
        , remove_last_from_path/1
        , set_allowed_errors/2
        , set_current_schema/2
        , set_error_list/2
        , set_validator_state/2
        , resolve_ref/2
        , undo_resolve_ref/2
        , canonical_path/2
        , combine_id/2
        ]).

-export_type([ state/0
             , validator_opts/0
             ]).

%% Includes
-include("jesse_schema_validator.hrl").

%% Internal datastructures
% -type http_uri_uri() :: string().

-record( state
       , { root_schema        :: jesse:schema()
         , current_schema     :: jesse:schema()
         , current_path       :: current_path()
                                 %% current path in reversed order
         , allowed_errors     :: jesse:allowed_errors()
         , error_list         :: jesse:error_list()
         , error_handler      :: jesse:error_handler()
         , validator      = undefined    :: module() | 'undefined'
         , validator_state    = undefined :: any() | 'undefined'
         , default_schema_ver :: jesse:schema_ver()
         , schema_loader_fun  :: jesse:schema_loader_fun()
         , id                 :: jesse:schema_id()
         }
       ).

%% current path in reversed order
-type current_path() :: [current_path_item()].
-type current_path_item() :: binary() | non_neg_integer().

-opaque state() :: #state{}.

-type validator_opts() :: any().

%%% API
%% @doc Adds `Property' to the `current_path' in `State'.
-spec add_to_path( State :: state()
                 , Item :: current_path_item()
                 ) -> state().
add_to_path(State, Item) ->
  CurrentPath = State#state.current_path,
  State#state{current_path = [Item | CurrentPath]}.

%% @doc Getter for `allowed_errors'.
-spec get_allowed_errors(State :: state()) -> jesse:allowed_errors().
get_allowed_errors(#state{allowed_errors = AllowedErrors}) ->
  AllowedErrors.

%% @doc Getter for `current_path'.
-spec get_current_path(State :: state()) -> current_path().
get_current_path(#state{current_path = CurrentPath}) ->
  CurrentPath.

%% @doc Getter for `current_schema'.
-spec get_current_schema(State :: state()) -> jesse:schema().
get_current_schema(#state{current_schema = CurrentSchema}) ->
  CurrentSchema.

%% @doc Getter for `current_schema_id'.
-spec get_current_schema_id(State :: state()) -> jesse:schema_id().
get_current_schema_id(#state{ current_schema = CurrentSchema
                            , root_schema = RootSchema
                            }) ->
  Default = jesse_json_path:value(?ID, RootSchema, ?not_found),
  jesse_json_path:value(?ID, CurrentSchema, Default).

%% @doc Getter for `default_schema_ver'.
-spec get_default_schema_ver(State :: state()) -> jesse:schema_ver().
get_default_schema_ver(#state{default_schema_ver = SchemaVer}) ->
  SchemaVer.

%% @doc Getter for `validator'.
-spec get_validator(State :: state()) -> module() | undefined.
get_validator(#state{validator = Validator}) ->
  Validator.

%% @doc Getter for `validator_state'.
-spec get_validator_state(State :: state()) -> any() | undefined.
get_validator_state(#state{validator_state = ValidatorState}) ->
  ValidatorState.

%% @doc Getter for `error_handler'.
-spec get_error_handler(State :: state()) -> jesse:error_handler().
get_error_handler(#state{error_handler = ErrorHandler}) ->
  ErrorHandler.

%% @doc Getter for `error_list'.
-spec get_error_list(State :: state()) -> jesse:error_list().
get_error_list(#state{error_list = ErrorList}) ->
  ErrorList.

%% @doc Returns newly created state.
-spec new( JsonSchema :: jesse:schema()
         , Options :: jesse:options()
         ) -> state().
new(JsonSchema, Options) ->
  AllowedErrors = proplists:get_value( allowed_errors
                                     , Options
                                     , 0
                                     ),
  MetaSchemaVer = jesse_json_path:value( ?SCHEMA
                                       , JsonSchema
                                       , ?default_schema_ver
                                       ),
  DefaultSchemaVer = proplists:get_value( default_schema_ver
                                        , Options
                                        , MetaSchemaVer
                                        ),
  Validator        = proplists:get_value( validator
                                        , Options
                                        , undefined
                                        ),
  ValidatorOpts    = proplists:get_value( validator_opts
                                        , Options
                                        , undefined
                                        ),
  ValidatorState   = init_validator_state( Validator
                                         , ValidatorOpts
                                         ),
  LoaderFun        = proplists:get_value( schema_loader_fun
                                        , Options
                                        , ?default_schema_loader_fun
                                        ),

  ErrorHandler = proplists:get_value( error_handler
                                    , Options
                                    , ?default_error_handler_fun
                                    ),
  NewState = #state{ root_schema        = JsonSchema
                   , current_path       = []
                   , allowed_errors     = AllowedErrors
                   , error_list         = []
                   , validator          = Validator
                   , validator_state    = ValidatorState
                   , default_schema_ver = DefaultSchemaVer
                   , schema_loader_fun  = LoaderFun
                   , error_handler      = ErrorHandler
                   },
  set_current_schema(NewState, JsonSchema).

%% @doc Removes the last element from `current_path' in `State'.
-spec remove_last_from_path(State :: state()) -> state().
remove_last_from_path(State = #state{current_path = [_Property | Path]}) ->
  State#state{current_path = Path}.

%% @doc Setter for `allowed_errors'.
-spec set_allowed_errors( State :: state()
                        , AllowedErrors :: jesse:allowed_errors()
                        ) -> state().
set_allowed_errors(#state{} = State, AllowedErrors) ->
  State#state{allowed_errors = AllowedErrors}.

%% @doc Setter for `current_schema'.
-spec set_current_schema( State :: state()
                        , NewSchema :: jesse:schema()
                        ) -> state().
set_current_schema(#state{id = Id} = State, NewSchema) ->
  NewId = combine_id(Id, jesse_json_path:value(?ID, NewSchema, undefined)),
  State#state{current_schema = NewSchema, id = NewId}.

%% @doc Setter for `error_list'.
-spec set_error_list( State :: state()
                    , ErrorList :: jesse:error_list()
                    ) -> state().
set_error_list(State, ErrorList) ->
  State#state{error_list = ErrorList}.

%% @doc Setter for `validator_state'.
-spec set_validator_state(State :: state(), ValidatorState :: any()) -> state().
set_validator_state(State, ValidatorState) ->
  State#state{validator_state = ValidatorState}.

%% @doc Resolve a reference.
-spec resolve_ref(State :: state(), Reference :: jesse:schema_ref()) -> state().
resolve_ref(State, Reference) ->
  Id = State#state.id,
  CanonicalReference = combine_id(Id, Reference),
  [BaseURI | Pointer] = re:split( CanonicalReference
                                , <<$#>>
                                , [{return, binary}, unicode]
                                ),
  IsLocalReference =
    case Id of
      undefined ->
        BaseURI =:= <<"">>;
      _ ->
        binary_to_list(BaseURI) =:= Id
    end,
  case IsLocalReference of
    true ->
      Path = jesse_json_path:parse(Pointer),
      case load_local_schema(State#state.root_schema, Path) of
        ?not_found ->
          jesse_error:handle_schema_invalid({?schema_not_found, CanonicalReference}, State);
        Schema ->
          set_current_schema(State, Schema)
      end;
    false ->
      case load_schema(State, BaseURI) of
        ?not_found ->
          jesse_error:handle_schema_invalid({?schema_not_found, CanonicalReference}, State);
        RemoteSchema ->
          SchemaVer =
            jesse_json_path:value(?SCHEMA, RemoteSchema, ?default_schema_ver),
          NewState = State#state{ root_schema = RemoteSchema
                                , id = BaseURI
                                , default_schema_ver = SchemaVer
                                },
          Path = jesse_json_path:parse(Pointer),
          case load_local_schema(RemoteSchema, Path) of
            ?not_found ->
              jesse_error:handle_schema_invalid({?schema_not_found, CanonicalReference}, State);
            Schema ->
              set_current_schema(NewState, Schema)
          end
      end
  end.

%% @doc Revert changes made by resolve_reference.
-spec undo_resolve_ref(state(), state()) -> state().
undo_resolve_ref(RefState, OriginalState) ->
  RefState#state{ root_schema = OriginalState#state.root_schema
                , current_schema = OriginalState#state.current_schema
                , default_schema_ver = OriginalState#state.default_schema_ver
                , id = OriginalState#state.id
                }.

%% @doc Init custom validator state.
%% @private
-spec init_validator_state( Validator :: module() | undefined
                          , Opts :: validator_opts()
                          ) -> jesse_schema_validator:validator_state().
init_validator_state(undefined, _) ->
  undefined;
init_validator_state(Validator, Opts) ->
  Validator:init_state(Opts).

%% @doc Retrieve a specific part of a schema
%% @private
-spec load_local_schema( Schema :: ?not_found | jesse:schema()
                       , Path :: [binary()]
                       ) -> not_found | jesse:json_term().
load_local_schema(?not_found, _Path) ->
  ?not_found;
load_local_schema(Schema, []) ->
  case jesse_lib:is_json_object(Schema) of
    true ->
      Schema;
    false ->
      ?not_found
  end;
load_local_schema(Schema, [<<>> | Keys]) ->
  load_local_schema(Schema, Keys);
load_local_schema(Schema, [Key | Keys]) ->
  case jesse_lib:is_json_object(Schema) of
    true  ->
      SubSchema = jesse_json_path:value(Key, Schema, ?not_found),
      load_local_schema(SubSchema, Keys);
    false ->
      case jesse_lib:is_array(Schema) of
        true ->
          %% avoiding binary_to_integer to maintain R15 compatibility
          try list_to_integer(binary_to_list(Key)) of
            Index ->
              SubSchema = lists:nth(Index + 1, Schema),
              load_local_schema(SubSchema, Keys)
          catch
            _:_ -> ?not_found
          end;
        false ->
          ?not_found
      end
  end.

-type http_uri_uri() :: string() | unicode:unicode_binary(). %% From https://github.com/erlang/otp/blob/OTP-20.2.3/lib/inets/doc/src/http_uri.xml#L57

%% @doc Resolve a new id
%% @private
-spec combine_id(undefined | http_uri_uri(),
                 undefined | binary()) -> http_uri_uri().
combine_id(Id, undefined) ->
  Id;
combine_id(Id, RefBin) ->
  Ref = unicode:characters_to_list(RefBin),
  case http_uri:parse(Ref) of
    %% Absolute http/s:
    {ok, _} ->
      Ref;
    %% Absolute file:
    {error, {no_default_port, file, Ref}} ->
      Ref;
    %% Relative
    _ ->
      combine_relative_id(Id, Ref)
  end.

%% @doc Combine a relative id
%% @private
combine_relative_id(IdBin, RelId) when is_binary(IdBin) ->
  combine_relative_id(unicode:characters_to_list(IdBin), RelId);
combine_relative_id(undefined, RelId) ->
  RelId;
combine_relative_id(Id, "#" ++ Fragment) ->
  [WithoutFragment | _] = re:split(Id, "#", [{return, list}]),
  WithoutFragment ++ "#" ++ Fragment;
combine_relative_id(Id, RelId) ->
  Base = filename:dirname(Id),
  combine_relative_id2(Base, RelId).

%% @doc Combine a relative id
%% @private
combine_relative_id2("file:", RelId) ->
  canonical_path(RelId, "file:");
combine_relative_id2("file://" ++ Path, RelId) ->
  canonical_path(filename:join([Path, RelId]), "file:");
combine_relative_id2("http:", RelId) ->
  canonical_path(RelId, "http:");
combine_relative_id2("http://" ++ Path, RelId) ->
  canonical_path(Path ++ [$/, RelId], "http:");
combine_relative_id2("https:", RelId) ->
  canonical_path(RelId, "https:");
combine_relative_id2("https://" ++ Path, RelId) ->
  canonical_path(Path ++ [$/, RelId], "https:");
combine_relative_id2(".", RelId) ->
  canonical_path(RelId, "file:");
combine_relative_id2(Path, RelId) ->
  canonical_path(filename:join([Path, RelId]), "file:").

%% @doc Return a canonical URI path.
%% @private
canonical_path("file://" ++ Path, _) ->
  "file://" ++ filename:join(raw_canonical_path(Path));
canonical_path(Path, "file:" ++ _) ->
  "file://" ++ filename:join(raw_canonical_path(Path));
canonical_path("http://" ++ Path, _) ->
  "http://" ++ string:join(raw_canonical_path(Path), "/");
canonical_path(Path, "http:" ++ _) ->
  "http://" ++ string:join(raw_canonical_path(Path), "/");
canonical_path("https://" ++ Path, _) ->
  "https://" ++ string:join(raw_canonical_path(Path), "/");
canonical_path(Path, "https:" ++ _) ->
  "https://" ++ string:join(raw_canonical_path(Path), "/");
canonical_path(Path, _) ->
  "file://" ++ filename:join(raw_canonical_path(filename:absname(Path))).

%% @doc Return a raw canonical path.
%% @private
raw_canonical_path(Path) ->
  PathItems = re:split(Path, "\\\\|/", [{return, list}]),
  raw_canonical_path2(PathItems, []).

%% @doc Return a raw canonical path.
%% @private
raw_canonical_path2([], Acc) ->
    lists:reverse(Acc);
raw_canonical_path2([H|T], Acc) ->
  case H of
    "." ->
      raw_canonical_path2(T, Acc);
    ".." ->
      raw_canonical_path2(T, tl(Acc));
    _ ->
      raw_canonical_path2(T, [H|Acc])
  end.

%% @doc Load a schema based on URI
-spec load_schema( State :: state()
                 , SchemaURI :: string() | binary()
                 ) -> jesse:schema()
                    | ?not_found.
load_schema(State, SchemaURI) when is_binary(SchemaURI) ->
  load_schema(State, unicode:characters_to_list(SchemaURI));
load_schema(#state{schema_loader_fun = LoaderFun}, SchemaURI) ->
  try LoaderFun(SchemaURI) of
      {ok, Schema} ->
        Schema;
      Schema ->
        case jesse_lib:is_json_object(Schema) of
          true ->
            Schema;
          false ->
            ?not_found
        end
  catch
    _C:_E ->
      ?not_found
  end.
