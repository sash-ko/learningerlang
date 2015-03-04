-module(pgbadger_dispatcher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("deps/epgsql/include/epgsql.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, execute/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(executable, {sql, database}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

execute(E = #executable{sql=Sql, database=Database}) ->
	gen_server:call(?MODULE, {execute, E}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({execute, E = #executable{sql=Sql, database=Database}}, _From, State) ->

	{ok, Conn} = epgsql:connect("localhost", "dev", "", [
    						 	{database, "dev"},
    							{timeout, 4000}
							]),
	Result = epgsql:squery(Conn, Sql),
	ok = epgsql:close(Conn),

	{reply, {ok, Result}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

