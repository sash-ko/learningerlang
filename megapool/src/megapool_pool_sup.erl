-module(megapool_pool_sup).

-export([start_link/3, init/1]).

-behaviour(supervisor).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    MaxRestart = 1,
    MaxTime = 3000,
    {ok, {{one_for_all, MaxRestart, MaxTime},
          [{srv,
             {megapool_srv, start_link, [Name, Limit, self(), MFA]},
             permanent,
             5000, 
             worker,
             [megapool_srv]}]}}.
