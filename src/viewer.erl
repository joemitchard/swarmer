-module(viewer).
-author("JakePearse <jp480@kent.ac.uk>").

-behaviour(gen_server).

%%%% API
-export([start_link/0,get_zombies/1, get_humans/1, get_obs/1, get_items/1,
         update_zombies/2, update_humans/2, update_items/2, update_obs/2]).

%%%% gen_server callbacks
-export([code_change/3,handle_cast/2,handle_call/3,
handle_info/2,init/1,terminate/2]).

-record(state, {tile_map=maps:new(),
                zombie_map = maps:new(),
                human_map = maps:new(),
                item_map = maps:new(),
                obs_map = maps:new()}).

%%%%%%==========================================================================
%%%%%% API
%%%%%%==========================================================================

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%----------------------------------------------------------------------------
start_link() -> gen_server:start_link(?MODULE, [], []).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
update_zombies(Pid, {Tile,Entities}) ->
    gen_server:cast(Pid,{update_zombies,Tile,Entities}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
update_humans(Pid, {Tile,Entities}) ->
    gen_server:cast(Pid,{update_humans,Tile,Entities}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update items from the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
update_items(Pid, {Tile,Items}) ->
    gen_server:cast(Pid,{update_items,Tile,Items}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update the obslist of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
update_obs(Pid, {Tile, ObsList}) ->
    gen_server:cast(Pid,{update_obs,Tile,ObsList}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Returns the population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_zombies(Pid) ->
    gen_server:call(Pid,get_zombies).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Returns the population of the neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_humans(Pid) ->
    gen_server:call(Pid,get_humans).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Get items from neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_items(Pid) ->
    gen_server:call(Pid,get_items).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Get items from neighbourhood.
%%%% @end
%%%%----------------------------------------------------------------------------
get_obs(Pid) ->
    gen_server:call(Pid,get_obs).


%%%%%%==========================================================================
%%%%%% gen_server Callbacks
%%%%%%==========================================================================
init([]) -> 
   {ok, #state{}}. %new state record with default values

handle_cast({update_zombies, Tile, Entities}, #state{zombie_map = Zmap} = State) ->
    {noreply, State#state{zombie_map = maps:put(Tile,Entities,Zmap)}};

handle_cast({update_humans, Tile, Entities}, #state{human_map = Hmap} = State) ->
    {noreply, State#state{human_map = maps:put(Tile,Entities,Hmap)}};

handle_cast({update_items, Tile, Items}, #state{item_map = Imap} = State) ->
    {noreply, State#state{item_map = maps:put(Tile,Items,Imap)}};

handle_cast({update_obs, Tile, ObsList}, #state{obs_map = ObsMap} = State) ->
    {noreply, State#state{obs_map = maps:put(Tile,ObsList,ObsMap)}}.


handle_call(get_zombies,_From,State) ->
    Map = lists:flatten(maps:values(State#state.zombie_map)),
    {reply,Map,State};

handle_call(get_humans,_From,State) ->
    Map = lists:flatten(maps:values(State#state.human_map)),
    {reply,Map,State};

handle_call(get_items,_From,State) ->
    Map = lists:flatten(maps:values(State#state.item_map)),
    {reply,Map,State};

handle_call(get_obs,_From,State) ->
    Map = lists:flatten(maps:values(State#state.obs_map)),
    {reply,Map,State}.


% enxpected message
handle_info(Msg,State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
        {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.
