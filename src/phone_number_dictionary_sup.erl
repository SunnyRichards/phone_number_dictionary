%%%-------------------------------------------------------------------
%% @doc phone_number_dictionary top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(phone_number_dictionary_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1000, period => 3600},
    ChildSpecs = [

        #{
            id => dictionary_server,
            start => {dictionary_server, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => supervisor,
            modules => dynamic
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
