-module(pgbadger_dispatcher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, execute/1, add_database/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(executable, {sql, database_alias}).
-record(database, {db, user, password="", host="localhost", port=5432}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

execute(E = #executable{}) ->
	gen_server:call(?MODULE, {execute, E}).

add_database(DbAlias, Database=#database{}) ->
    gen_server:cast(?MODULE, {add_database, DbAlias, Database}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ets:new(databases, [set, named_table]),
    {ok, Args}.

handle_call({execute, E}, _From, State) ->
    case find_database(E#executable.database_alias) of
        {ok, Db} ->
    	   case execute_sql(Db, E#executable.sql) of
                {ok, Result} -> {reply, {ok, Result}, State};
                {error, Reason} -> {reply, {error, Reason}, State}
            end;
        {error, instance} ->
            {reply, {error, instance}, State}
    end.

handle_cast({add_database, DbAlias, Database}, State) ->
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

execute_sql(Db, Sql) ->
    case epgsql:connect(Db#database.host, Db#database.user, Db#database.password,
                        [{database, Db#database.db}, {timeout, 1000}]) of

        {ok, Conn} ->
            Result = epgsql:squery(Conn, Sql),
            ok = epgsql:close(Conn),
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

find_database(Alias) ->
    case ets:lookup(databases, Alias) of
        [{Alias, Db}] -> {ok, Db};
        [] -> {error, instance}
    end.

