-module(megapool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	%% The supervisor registered locally as "megapool"
    supervisor:start_link({local, megapool}, ?MODULE, []).

stop() ->
	case whereis(megapool) of
		P when is_pid(P) ->
			exit(P, kill);
		_ -> ok
	end. 


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

