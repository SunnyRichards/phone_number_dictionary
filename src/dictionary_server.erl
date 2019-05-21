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
-export([search_number/1]).

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
    gen_server:call(?MODULE, {search_number ,PhoneNumber}, infinity).

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

handle_call({search_number, PhoneNumber}, _From, State) ->
    Response = search(PhoneNumber),
    {reply, Response, State};

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
    Lists = split_number(PhoneNumString, PhoneNumString, 3, []),
    {Case1, RestList} = lists:split(3, Lists),
    {Case2, Case3} = lists:split(2, RestList),
    [get_word(list_to_binary(Num)) || Num <- Case1] ++
    [get_word(list_to_binary(Num)) || Num <- Case2] ++
    [get_word(list_to_binary(Num)) || Num <- Case3].

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
%% @spec split_number(Num, ActualNum, Length, Resp) -> SplitNumberList
%% @end
%%--------------------------------------------------------------------

-spec(split_number(Num :: list(), ActualNum  :: list(), Length :: integer(), Resp :: list()) ->
    SplitNumberList:: list()).
%%--------------------------------------------------------------------

split_number(Num, ActualNum, Length, Resp) when length(Num) > Length ->
    {List2, List3} = lists:split(Length, Num),
    case List3 of
        List3 when length(List3) > 3 ->
            split_number(List3, ActualNum, Length, Resp ++ [List2]);
        List3 ->
            split_number(ActualNum, ActualNum, Length +1, Resp)
    end;

split_number(_, ActualNum, Length, Resp) when Length =:= 10 ->
    Resp ++ [ActualNum];

split_number(Num, ActualNum, Length, Resp) ->
    split_number(ActualNum, ActualNum, Length +1, Resp ++ [Num]).

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