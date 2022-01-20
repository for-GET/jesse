-module(prop_get_equal).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_get_equal() ->
    ?FORALL(Type, resize(4, json_list()),
        begin
            boolean(Type)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(Type) ->
    jesse_lib:is_equal(
        jesse_lib:normalize_and_sort(Type),
        Type).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
json_list() -> list(proper_json:json()).
