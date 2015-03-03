-module(megapool).
-export([start_link/0, stop/0, start_pool/3,
			run/2, sync_queue/2, async_queue/2, stop_pool/1]).
 
start_link() ->
	megapool_sup:start_link().
 
stop() ->
	megapool_sup:stop().
 
start_pool(Name, Limit, {M,F,A}) ->
	megapool_sup:start_pool(Name, Limit, {M,F,A}).
 
stop_pool(Name) ->
	megapool_sup:stop_pool(Name).
 
run(Name, Args) ->
	megapool_srv:run(Name, Args).
 
async_queue(Name, Args) ->
	megapool_srv:async_queue(Name, Args).
 
sync_queue(Name, Args) ->
	megapool_srv:sync_queue(Name, Args).