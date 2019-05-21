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
-export([convert_to_list/1, run_concurrent/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gives back list or string for given any data type
%%
%% @spec convert_to_list(Value) -> Response
%% @end
%%--------------------------------------------------------------------

-spec(convert_to_list(Value :: term()) ->
    Response :: list()).
%%--------------------------------------------------------------------

convert_to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);

convert_to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);

convert_to_list(Value) when is_float(Value) ->
    float_to_list(Value);

convert_to_list(Value) when is_list(Value) -> Value.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Run concurrent tasks in parallel and collect all results
%%
%% @spec run_concurrent(Tasks, TimeoutMilliSec) -> Response
%% @end
%%--------------------------------------------------------------------

-spec run_concurrent(Tasks :: [{F :: function(), Args :: list()}],
    TimeoutMilliSec :: non_neg_integer()) -> list().
%%--------------------------------------------------------------------

run_concurrent(Tasks, TimeoutMilliSec) when TimeoutMilliSec >= 0 ->
    ParentPid = self(),
    Ref = erlang:make_ref(),
    PidList = lists:foldl(fun(E, AccIn) ->
        WPid = erlang:spawn_link(
            fun() ->
                R = case E of
                        {F, A} ->
                            apply(F, A);
                        {M, F, A} ->
                            apply(M, F, A)
                    end,
                ParentPid ! {Ref, self(), R}
            end),
        [WPid | AccIn]
                       end, [], Tasks),
    OldTrapExitFlag = erlang:process_flag(trap_exit, true),
    Result = receive_concurrent_tasks(Ref, PidList, TimeoutMilliSec, []),
    erlang:process_flag(trap_exit, OldTrapExitFlag),
    Result.

receive_concurrent_tasks(_Ref, [], _TimeoutMilliSec, AccIn) ->
    {ok, AccIn};
receive_concurrent_tasks(Ref, PidList, TimeoutMilliSec, AccIn) ->
    T1 = erlang:system_time(millisecond),
    receive
        {Ref, Pid, R} ->
            %% This is time consuming since it is O(N),
            %% but this interface shall not be invoked for very high
            %% concurrency.
            RemainingPids = PidList -- [Pid],
            T2 = erlang:system_time(millisecond),
            %% Notice that some time has elapsed, so account for that
            receive_concurrent_tasks(Ref, RemainingPids,
                TimeoutMilliSec - (T2 - T1),
                [R | AccIn])
    after
        TimeoutMilliSec ->
            lists:foreach(fun(P) ->
                erlang:exit(P, kill)
                          end, PidList),
            {error, {timeout, AccIn}}
    end.