-module(pg_worker_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(DbAlias, Limit) ->
	SupName = list_to_atom(atom_to_list(?MODULE) ++ DbAlias),
    supervisor:start_link({local, SupName}, ?MODULE, {DbAlias, Limit}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({DbAlias, Limit}) ->
	MaxRestart = 5,
	MaxTime = 1000,
	ChildSpecs = lists:map(
        fun (Id) ->
            {worker_name(DbAlias, Id),
                {pg_worker, start_worker, [DbAlias]},
                	permanent, 2000, worker, [pg_worker]}
        end, lists:seq(0, Limit-1)),
    {ok, { {one_for_one, MaxRestart, MaxTime}, ChildSpecs} }.

worker_name(Predix, Id) ->
 	lists:flatten([Predix |
 		["_worker_" | io_lib:format("~p", [Id])]]).