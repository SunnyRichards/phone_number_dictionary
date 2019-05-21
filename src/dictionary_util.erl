%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. May 2019 10:58 AM
%%%-------------------------------------------------------------------
-module(dictionary_util).
-author("sunnyrichards").

%% API
-export([convert_to_list/1]).

convert_to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);

convert_to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);

convert_to_list(Value) when is_float(Value) ->
    float_to_list(Value);

convert_to_list(Value) when is_list(Value) -> Value.