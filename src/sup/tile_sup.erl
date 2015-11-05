-module(tile_sup).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	{ok,{{simple_one_for_one,1,60},
  		[{normal_tile, {tile,start_link,[]},
    	permanent,brutal_kill,worker,[tile]}]}}.
