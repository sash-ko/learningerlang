-module(dbregister).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("pgbadger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, add_database/2, find_database/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_database(Alias, Database=#database{}) ->
	gen_server:cast(?MODULE, {add_database, Alias, Database}).

find_database(Alias) ->
	gen_server:call(?MODULE, {find_database, Alias}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	ets:new(databases, [set, named_table]),
    {ok, Args}.

handle_call({find_database, Alias}, _From, State) ->
	case ets:lookup(databases, Alias) of
        [{Alias, Db}] -> {reply, {ok, Db}, State};
        [] -> {reply, {error, instance}, State}
    end.

handle_cast({add_database, DbAlias, Database}, State) ->
    pgbadger_sup:start_pool(DbAlias, 10),
    ets:insert(databases, {DbAlias, Database}),
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


