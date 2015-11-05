-module(zombie_sup).
-author("Robert Hales rsjh3@kent.ac.uk").

-behaviour(supervisor).
 
-export([start_link/0]).
-export([init/1]).
 
start_link() ->
supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
{ok,{{simple_one_for_one,1,60},
  [{normal_zombie, {zombie_fsm,start_link,[]},
    temporary,500,worker,[zombie_fsm]}]}}.
