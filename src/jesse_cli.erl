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
%% @doc JESSE (JSon Schema Erlang)
%%
%% This is an interface module which provides an access to the main
%% functionality of jesse, such as 1) updating of the schema definitions cache;
%% 2) validation json data against a schema.
%% @end
%%%=============================================================================

%%%_* Module declaration =======================================================
-module(jesse_cli).

%%%_* Exports ==================================================================
%% API
-export([main/1]).

%%%_* API ======================================================================

main([]) ->
  main(["--help"]);
main(["-h"]) ->
  main(["--help"]);
main(["--help"]) ->
  io:fwrite( "Usage: ~s [--json] path_to_json_schema -- "
             "path_to_json_instance [path_to_json_instance] ~n"
           , [escript:script_name()]
           );
main(Options) ->
  main(Options, [], [], []).

main(["--json"|Rest], Options, [], []) ->
  main(Rest, Options ++ [{json, true}], [], []);

main(["--"|JsonInstances], Options, Schemata, []) ->
  run(Options, Schemata, JsonInstances);
main([Schema|Rest], Options, Schemata, []) ->
  main(Rest, Options, [Schema|Schemata], []).

%%%_* Internal =================================================================

run(_Options, _Schemata, []) ->
  ok;
run(Options, [Schema|_] = Schemata, [JsonInstance|JsonInstances]) ->
  JesseResult = jesse_run(JsonInstance, Schema),
  Result = case JesseResult of
             {ok, _} ->
               [ {filename, list_to_binary(JsonInstance)}
               , {result, ok}
               ];
             {error, Reasons} ->
               JsxReasons = lists:map(fun jesse_error:reason_to_jsx/1, Reasons),
               [ {filename, list_to_binary(JsonInstance)}
               , {result, error}
               , {errors, JsxReasons}
               ]
           end,
  case proplists:get_value(json, Options) of
    undefined ->
      io:fwrite("~p\n\n", [Result]);
    true ->
      io:fwrite("~s\n\n", [jsx:encode(Result)])
  end,
  case JesseResult of
    {ok, _} ->
      run(Options, Schemata, JsonInstances);
    _ ->
      %% init:stop not setting status code correctly
      %% init:stop(1)
      halt(1)
  end.

jesse_run(JsonInstance, Schema) ->
  %% Don't use application:ensure_all_started(jesse)
  %% nor application:ensure_started(_)
  %% in order to maintain compatibility with R16B01 and lower
  ok = ensure_started(jsx),
  ok = ensure_started(jesse),
  {ok, JsonInstanceBinary} = file:read_file(JsonInstance),
  {ok, SchemaBinary} = file:read_file(Schema),
  jesse:validate_with_schema( SchemaBinary
                            , JsonInstanceBinary
                            , [{parser_fun, fun jsx:decode/1}]
                            ).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
