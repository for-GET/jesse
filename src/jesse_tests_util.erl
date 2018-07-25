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
-define(OPTIONS,     <<"options">>).
-define(SCHEMA,      <<"schema">>).
-define(TESTS,       <<"tests">>).
-define(VALID,       <<"valid">>).

-ifdef(OTP_RELEASE). %% OTP 21+
-define(EXCEPTION(C, R, Stacktrace), C:R:Stacktrace ->).
-else.
-define(EXCEPTION(C, R, Stacktrace), C:R -> Stacktrace = erlang:get_stacktrace(),).
-endif.

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
                     Options = get_path(?OPTIONS, Test),
                     ct:pal( "** Description: ~s~n"
                             "** Options: ~p~n"
                             "** Schema: ~p~n"
                             "** Schema tests: ~p~n"
                           , [Description, Options, Schema, SchemaTests]
                           ),
                     test_schema(DefaultSchema, Options, Schema, SchemaTests)
                 end
               , Tests
               ).

%%% Internal functions

test_schema(DefaultSchema, Opts0, Schema, SchemaTests) ->
  Opts1 = make_options(Opts0),
  lists:foreach( fun(Test) ->
                     Description = get_path(?DESCRIPTION, Test),
                     Instance = get_path(?DATA, Test),
                     ct:pal("* Test case: ~s~n", [Description]),
                     Opts = [ {default_schema_ver, DefaultSchema}
                            , {schema_loader_fun, fun load_schema/1}
                            ] ++ Opts1,
                     try jesse:validate_with_schema(Schema, Instance, Opts) of
                         Result ->
                         ct:pal("Result: ~p~n", [Result]),
                         case get_path(?VALID, Test) of
                           true ->
                             {ok, Instance} = Result;
                           false ->
                             {error, _} = Result
                         end
                     catch ?EXCEPTION(C,R,Stacktrace)
                         ct:pal( "Error: ~p:~p~n"
                                 "Stacktrace: ~p~n"
                               , [C, R, Stacktrace]
                               )
                     end
                 end
               , SchemaTests
               ).

make_options(Options) ->
  lists:map( fun ({Key0, Value0}) ->
                 Key = case is_binary(Key0) of
                         true -> list_to_existing_atom(binary_to_list(Key0));
                         false -> Key0
                       end,
                 Value = case is_binary(Value0) of
                         true -> list_to_existing_atom(binary_to_list(Value0));
                         false -> Value0
                       end,
                 {Key, Value}
             end
           , Options
           ).

testfile_to_key(TestFile) ->
  filename:rootname(filename:basename(TestFile)).

get_path(Key, Schema) ->
  jesse_json_path:path(Key, Schema).

load_schema(URI) ->
  {ok, Response} = httpc:request(get, {URI, []}, [], [{body_format, binary}]),
  {{_Line, 200, _}, _Headers, Body} = Response,
  jsx:decode(Body).
