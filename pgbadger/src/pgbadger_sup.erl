-module(pgbadger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_pool/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_pool(DbAlias, Limit) ->
	ChildSpec = {
		DbAlias,
		{pgbadger_pool_sup, start_link, [DbAlias, Limit]},
		permanent,
		10500,
		supervisor,
		[pgbadger_pool_sup]
	},
	supervisor:start_child(?MODULE, ChildSpec).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	MaxRestart = 5,
	MaxTime = 1000,
	DispatcherSpec = ?CHILD(pgbadger_dispatcher, worker),
    {ok, { {one_for_all, MaxRestart, MaxTime}, [DispatcherSpec]} }.