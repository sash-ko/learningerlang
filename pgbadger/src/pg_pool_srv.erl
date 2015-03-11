-module(pg_pool_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2, add_worker/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(pool_state, {name, size=0, workers=queue:new()}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DbAlias, Limit) ->
    gen_server:start_link({local, pool_name(DbAlias)}, ?MODULE, {DbAlias, Limit}, []).

add_worker(DbAlias, Pid) ->
	gen_server:cast(pool_name(DbAlias), {add_worker, Pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({DbAlias, Limit}) ->
    {ok, #pool_state{name=DbAlias, size=Limit}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_worker, Pid}, State) ->
	erlang:monitor(process, Pid),
	Workers = queue:in(Pid, State#pool_state.workers),
    {noreply, State#pool_state{workers=Workers}}.

%% handle worker that goes down
handle_info({'DOWN', _, _, Pid, _}, State) ->
	Workers = queue:filter(fun(W) -> W =/= Pid end, State#pool_state.workers),
    {noreply, State#pool_state{workers=Workers}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

pool_name(Name) ->
	list_to_atom(atom_to_list(?MODULE) ++ Name).
