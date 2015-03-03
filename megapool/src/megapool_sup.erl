-module(megapool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% Start high level supervisor
start_link() ->
	%% The supervisor registered locally as "megapool"
    supervisor:start_link({local, megapool}, ?MODULE, []).

stop() ->
	case whereis(megapool) of
		P when is_pid(P) ->
			exit(P, kill);
		_ -> ok
	end. 

%% Start individual pool supervisor
%% MFA - module, function, arguments
start_pool(Name, Limit, MFA) ->
	ChildSpec = {
		Name,
		{megapool_pool_sup, start_link, [Name, Limit, MFA]},
		permanent,
		10500,
		supervisor,
		[megapool_pool_sup]
	},
	supervisor:start_child(megapool, ChildSpec).

stop_pool(Name) ->
	supervisor:terminate_child(megapool, Name),
	supervisor:delete_child(megapool, Name).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	MaxRestart = 5,
	MaxTime = 3600,
    {ok, { {one_for_one, MaxRestart, MaxTime}, []} }.
