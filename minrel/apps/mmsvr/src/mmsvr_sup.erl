%%%-------------------------------------------------------------------
%% @doc mmsvr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mmsvr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export ([start_child/3]).

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
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    Procs = [#{id => match_queue,
        start => {match_queue, start, []},
        restart => permanent, 
        shutdown => brutal_kill, 
        type => worker,
        modules => [match_queue]},

        #{id => players,
        start => {players, start, []},
        restart => permanent, 
        shutdown => brutal_kill, 
        type => worker,
        modules => [players]}],
    {ok, {SupFlags, Procs}}.

start_child(M, F, A) ->
	supervisor:start_child(?MODULE,
     {erlang:system_time(), {M, F, A}, permanent, brutal_kill, worker, [M]}).

%%====================================================================
%% Internal functions
%%====================================================================
