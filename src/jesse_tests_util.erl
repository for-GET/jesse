%%%=============================================================================
%% Copyright 2016- AUTHORS
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
%% @doc jesse test utility functions
%% @end
%%%=============================================================================

-module(jesse_tests_util).

%% API
-export([ get_tests/2
        , do_test/2
        ]).

-include_lib("common_test/include/ct.hrl").

%% JSON-Schema-Test-Suite attributes definitions
-define(DATA,        <<"data">>).
-define(DESCRIPTION, <<"description">>).
-define(SCHEMA,      <<"schema">>).
-define(TESTS,       <<"tests">>).
-define(VALID,       <<"valid">>).

%%% API

get_tests(RelativeTestsDir, DefaultSchema) ->
  TestsDir = filename:join( os:getenv("TEST_DIR")
                          , RelativeTestsDir
                          ),
  TestFiles = filelib:wildcard(TestsDir ++ "/*.json"),
  lists:map( fun(TestFile) ->
                 {ok, Bin} = file:read_file(TestFile),
                 Tests = jsx:decode(Bin),
                 Key = testfile_to_key(TestFile),
                 Config = {Tests, DefaultSchema},
                 {Key, Config}
             end
           , TestFiles
           ).

do_test(Key, Config) ->
  {Tests, DefaultSchema} = ?config(Key, Config),
  lists:foreach( fun(Test) ->
                     Description = get_path(?DESCRIPTION, Test),
                     Schema = get_path(?SCHEMA, Test),
                     SchemaTests = get_path(?TESTS, Test),
                     ct:pal( "** Description: ~s~n"
                             "** Schema: ~p~n"
                             "** Schema tests: ~p~n"
                           , [Description, Schema, SchemaTests]
                           ),
                     test_schema(DefaultSchema, Schema, SchemaTests)
                 end
               , Tests
               ).

%%% Internal functions

get_path(Key, Schema) ->
  jesse_lib:get_path(Key, Schema).

test_schema(DefaultSchema, Schema, SchemaTests) ->
  lists:foreach( fun(Test) ->
                     Description = get_path(?DESCRIPTION, Test),
                     Instance = get_path(?DATA, Test),
                     ct:pal("* Test case: ~s~n", [Description]),
                     Opts = [ {default_schema_ver, DefaultSchema}
                            , {schema_loader_fun, fun load_schema/1}
                            ],
                     try jesse:validate_with_schema(Schema, Instance, Opts) of
                         Result ->
                         ct:pal("Result: ~p~n", [Result]),
                         case get_path(?VALID, Test) of
                           true ->
                             {ok, Instance} = Result;
                           false ->
                             {error, _} = Result
                         end
                     catch C:E ->
                         ct:pal( "Error: ~p:~p~n"
                                 "Stacktrace: ~p~n"
                               , [C, E, erlang:get_stacktrace()]
                               )
                     end
                 end
               , SchemaTests
               ).

testfile_to_key(TestFile) ->
  filename:rootname(filename:basename(TestFile)).

load_schema(URI) ->
  {ok, Response} = httpc:request(get, {URI, []}, [], [{body_format, binary}]),
  {{_Line, 200, _}, _Headers, Body} = Response,
  jsx:decode(Body).
