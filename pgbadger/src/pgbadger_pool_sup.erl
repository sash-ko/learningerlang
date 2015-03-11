-module(pgbadger_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(DbAlias, Limit) ->
	SupName = list_to_atom(atom_to_list(?MODULE) ++ DbAlias),
    supervisor:start_link({local, SupName}, ?MODULE, [DbAlias, Limit]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([DbAlias, Limit]) ->
	MaxRestart = 5,
	MaxTime = 1000,
	WorkerPoolSupSpec = {
		pg_worker_pool,
		{pg_worker_pool_sup, start_link, [DbAlias, Limit]},
		permanent,
		10500,
		supervisor,
		[pool_sup]
	},
	PoolSrvSpec = {
		pg_pool_srv,
		{pg_pool_srv, start_link, [DbAlias, Limit]},
		permanent,
		10500,
		worker,
		[pool_sup]
	},
    {ok, { {one_for_one, MaxRestart, MaxTime}, [PoolSrvSpec, WorkerPoolSupSpec]} }.