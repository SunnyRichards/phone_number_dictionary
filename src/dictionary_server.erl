%%%-------------------------------------------------------------------
%%% @author sunnyrichards
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. May 2019 10:31 AM
%%%-------------------------------------------------------------------
-module(dictionary_server).
-author("sunnyrichards").

-behaviour(gen_server).
-include("dictionary.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).
-export([search_number/1, get_word/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
search_number(PhoneNumber) ->
    StartTime = erlang:monotonic_time(millisecond),
    gen_server:call(?MODULE, {search_number ,PhoneNumber, StartTime}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    Directory = code:priv_dir(?APPLICATION_NAME),
    {ok, DataBin} = file:read_file(Directory ++ "/dictionary.txt"),
    DataBinList = binary:split(DataBin, <<"\n">>, [global]),
    DictRecList = make_record(DataBinList, []),
    ets:new(dictionary, [bag, {keypos,#dict.word}, named_table]),
    ets:insert(dictionary, DictRecList),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({search_number, PhoneNumber, StartTime}, _From, State) ->
    Response = search(PhoneNumber),
    EndTime = erlang:monotonic_time(millisecond),
    {reply, {(EndTime - StartTime)/1000, Response}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call by gen server to fetch the dictionary words for the given
%% phone number
%%
%% @spec search(PhoneNumber) -> Response
%% @end
%%--------------------------------------------------------------------

-spec(search(PhoneNumber :: term()) ->
    Response :: list()).
%%--------------------------------------------------------------------

search(PhoneNumber) ->
    PhoneNumString = dictionary_util:convert_to_list(PhoneNumber),
    Lists = split_number(PhoneNumString),
    Tasks = [{dictionary_server, get_word, [list_to_binary(Num)]} || Num <- Lists],
    {ok, ResponseList} = dictionary_util:run_concurrent(Tasks, 1200000),
    validate_and_form_response(lists:flatten(ResponseList), []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server init where all the dictionary
%% words along with their numerical value will be converted into
%% pre-defined record lists to be stored in the ets table
%%
%% @spec make_record(WordsList, RecordList) -> RecordList.
%% @end
%%--------------------------------------------------------------------

-spec(make_record(WordsList :: list(binary()), RecordList :: list()) ->
    RecordList :: list(tuple())).
%%--------------------------------------------------------------------

make_record([], RecordList) -> RecordList;

make_record([Word|Others], RecordList) ->
    KeyMap =  [{2, [$A, $B, $C]}, {3, [$D, $E, $F]}, {4,[$G, $H, $I]}, {5, [$J, $K, $L]},
        {6, [$M, $N, $O]}, {7, [$P, $Q, $R, $S]}, {8, [$T, $U, $V]}, {9, [$W, $X, $Y, $Z]}],

    NumericalBin =  lists:foldl(fun(Character, Rem) ->
        Fun = fun({Key, ValueList}, Resp) ->
            case lists:member(Character, ValueList) of
                true ->
                    Data = integer_to_binary(Key),
                    <<Rem/binary, Data/binary>>;
                false -> Resp
            end end,
        lists:foldl(fun(Data, Rem) ->
            Fun(Data, Rem) end, Rem, KeyMap)  end, <<>>, binary_to_list(Word)),

    Record = {dict, Word, NumericalBin},
    make_record(Others, [Record|RecordList]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will split the 10 digit phone number for all the
%% possible and valid lengths
%%
%% @spec split_number(Num) -> SplitNumberList
%% @end
%%--------------------------------------------------------------------

-spec(split_number(Num :: list()) ->
    SplitNumberList:: list()).
%%--------------------------------------------------------------------

split_number(Num) ->
    lists:foldl(fun(List, Resp) ->
        Resp ++ split(List, Num, []) end, lists:foldl(fun(Split, Response) ->
        {Num1, Num2} = lists:split(Split, Num),
        Response ++ [Num1] ++ [Num2] end, [Num], lists:seq(3,7)), get_other_combinations(10, 3, [])).

get_other_combinations(Number, Limit, Resp) when Number >= Limit ->
    case Number - Limit of
        Value when Value >= Limit->
            get_other_combinations(Value, Limit, [Limit|Resp]);
        _ ->
            lists:usort(perms([Number|Resp]))
    end.

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].

split([], [], Resp) -> Resp;

split([SplitNum|Rest], Num, Resp) ->
    {Num1, Num2} = lists:split(SplitNum, Num),
    split(Rest, Num2, Resp ++ [Num1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return the matched dictionary word for the
%% corresponding numerical map after searching it from the ets table
%%
%% @spec get_word(Number) -> DictionaryWord
%% @end
%%--------------------------------------------------------------------

-spec(get_word(Number :: binary()) -> DictionaryWord :: binary()).
%%--------------------------------------------------------------------

get_word(Number) ->
    ets:select(dictionary, ets:fun2ms(fun(Dictionary = #dict{number_map = NumberMap})
        when NumberMap =:= Number -> Dictionary#dict.word end)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will return valid response removing the unpaired words
%% from the dictionary and also gives all the combinations of the words
%%
%% @spec validate_and_form_response(Number :: , Resp) -> DictionaryWord
%% @end
%%--------------------------------------------------------------------

-spec(validate_and_form_response(Number :: binary(), Resp :: list()) -> DictionaryWord :: binary()).
%%--------------------------------------------------------------------
validate_and_form_response([], Resp) ->
    lists:foldl(fun(ElementList, Response) ->
        case lists:member(no_match, ElementList) of
            true ->
                Response;
            false ->
                Response ++ [ElementList]
        end end, [], Resp);

validate_and_form_response([Head|Rest], Resp) ->
    case 10 - byte_size(Head)of
        0 ->
            validate_and_form_response(Rest, Resp ++ [[Head]]);
        NextWordLength ->
            {Length1, Length2} = next_word_size(NextWordLength),
            validate_and_form_response(Rest, Resp ++ [[Head, find_other_word(NextWordLength, Rest)]] ++
                [[Head, find_other_word(Length1, Rest), find_other_word(Length2, Rest)]])
    end.

find_other_word(_, []) -> no_match;

find_other_word(NextWordLength, [Word|Rest]) ->
    case byte_size(Word) =:= NextWordLength of
        true ->
            Word;
        false ->
            find_other_word(NextWordLength, Rest)
    end.

next_word_size(NextWordLength) when NextWordLength rem 2 =:= 0 andalso NextWordLength >= 6-> {3,3};
next_word_size(NextWordLength) when NextWordLength >= 6 -> {3,4};
next_word_size(_) -> {0,0}.