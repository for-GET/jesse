%% @author Nicholas Lundgaard <nalundgaard@gmail.com>

%% @doc EUnit tests for jesse_lib.
-module(jesse_lib_tests).

-include_lib("eunit/include/eunit.hrl").

re_run_default_test_() ->
  {setup,
   fun() ->
     application:load(jesse)
   end,
   fun(_) ->
     application:unload(jesse)
   end,
   [
    {"Support ISO Latin-1 letters in \\w by default",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"föø"/utf8>>, "^\\w+$"))},
    {"Support ISO Latin-1 numbers in \\d by default",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"123"/utf8>>, "^\\d+$"))},
    {"Support beyond ISO Latin-1 letters in \\w by default",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"fōô"/utf8>>, "^\\w+$"))},
    {"Support beyond ISO Latin-1 numbers in \\d by default",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"3๓३"/utf8>>, "^\\d+$"))}
   ]}.

-if(defined(OTP_RELEASE) andalso ?OTP_RELEASE >= 28).
re_run_no_ucp_test_() ->
  {setup,
   fun() ->
     application:load(jesse),
     application:set_env(jesse, re_options, [unicode])
   end,
   fun(_) -> application:unload(jesse) end,
   [
    {"Do not support ISO Latin-1 letters in \\w without 'ucp'",
     ?_assertEqual(nomatch,
                   jesse_lib:re_run(<<"föø"/utf8>>, "^\\w+$"))},
    {"Support ISO Latin-1 numbers in \\d  without 'ucp'",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"123"/utf8>>, "^\\d+$"))},
    {"Do not support beyond ISO Latin-1 letters in \\w without 'ucp'",
     ?_assertEqual(nomatch,
                   jesse_lib:re_run(<<"fōô"/utf8>>, "^\\w+$"))},
    {"Do not support beyond ISO Latin-1 numbers in \\d without 'ucp'",
     ?_assertEqual(nomatch,
                   jesse_lib:re_run(<<"3๓३"/utf8>>, "^\\d+$"))}
   ]}.
-else.
re_run_no_ucp_test_() ->
  {setup,
   fun() ->
     application:load(jesse),
     application:set_env(jesse, re_options, [unicode])
   end,
   fun(_) -> application:unload(jesse) end,
   [
    {"Support ISO Latin-1 letters in \\w without 'ucp'",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"föø"/utf8>>, "^\\w+$"))},
    {"Support ISO Latin-1 numbers in \\d  without 'ucp'",
     ?_assertEqual(match,
                   jesse_lib:re_run(<<"123"/utf8>>, "^\\d+$"))},
    {"Do not support beyond ISO Latin-1 letters in \\w without 'ucp'",
     ?_assertEqual(nomatch,
                   jesse_lib:re_run(<<"fōô"/utf8>>, "^\\w+$"))},
    {"Do not support beyond ISO Latin-1 numbers in \\d without 'ucp'",
     ?_assertEqual(nomatch,
                   jesse_lib:re_run(<<"3๓३"/utf8>>, "^\\d+$"))}
   ]}.
-endif.
