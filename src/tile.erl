-module(tile).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(gen_server).

%%%% API
-export([start_link/4]).

%%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%% tile functions
-export([summon_entity/2,
        remove_entity/3,
        update_entity/4,
        get_geometry/1,
        set_viewer/2,
        get_viewer/1,
        set_neighbours/2,
        get_neighbours/1,
        terminate/1,
        get_state/1,
        place_item/2,
        remove_item/2,
        check_obs/2,
        set_obs_list/2,
        validmove/5]).


-define(SERVER, ?MODULE).

-define(SPEED_HARD_LIMIT,5).
-define(ENT_REFLECT_RATE,1).
-define(OBS_REFLECT_RATE,2).

-type   coord() ::  pos_integer().
-type   pos()  ::  {pos_integer(),pos_integer()}.
-type   entity()  ::  {pid(),{pos_integer(),pos_integer()}}.

%%%% entity_map - a dictonary of entities within the tile
%%%% x and y origin - the origin of the tile
%%%% x and y limit - the edge of the tile
%%%% coords - a tuple containing {Xo,Yo, Xl,Yl}
%%%% viewer - the assigned viewer of the tile
%%%% neihbours - a list of the neighbouring tiles viewers

-record(state, {zombie_map=maps:new() :: maps:maps(),
                human_map=maps:new() :: maps:maps(),
                item_map=maps:new() :: maps:maps(),
                xorigin  ::  coord(),
                yorigin  ::  coord(),
                xlimit  ::  coord(),
                ylimit  ::  coord(),
                coords  ::  tuple(),
                viewer  ::  pid(),
                neighbours  ::  [pid()],
                obs_list=[] :: list()}). 

%%%%%%==========================================================================
%%%%%% API
%%%%%%==========================================================================

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec start_link(atom(), coord(),coord(),pos_integer()) -> ok.
start_link(Name,X,Y,Size) ->
    gen_server:start_link(?MODULE, [Name,X,Y,Size], []).

%%%%-Calls----------------------------------------------------------------------
%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the geometry of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_geometry(pid()) -> ok.
get_geometry(Pid) ->
    gen_server:call(Pid, get_geometry).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the assigned viewer of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_viewer(pid()) -> ok.
get_viewer(Pid) ->
    gen_server:call(Pid, get_viewer).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return a list of the associated viewers from the neighbouring tiles.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_neighbours(pid()) -> ok.
get_neighbours(Pid) ->
    gen_server:call(Pid, get_neighbours).


set_obs_list(Pid,New_obs_list) ->
	gen_server:call(Pid,{set_obs_list,New_obs_list}).

%%% provides api call to check if a pos() is obstructed
check_obs(Pid,Pos) ->
	gen_server:call(Pid,{check_obs,Pos}).
	
%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Update the entities position on the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
%-spec update_entity(pid(),entity(),pos(),_,_,_) -> ok.
update_entity(Pid, Entity, NewPos, Velocity) ->
    gen_server:call(Pid, {update_entity, Entity, NewPos,Velocity}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Place an item on the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
place_item(Pid, Item) ->
    gen_server:call(Pid, {place_item, Item}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Remove an item from the tile
%%%% @end
%%%%----------------------------------------------------------------------------
remove_item(Pid, Item) ->
    gen_server:call(Pid, {remove_item, Item}).

%%%%-Casts----------------------------------------------------------------------

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Add an entity to the tile.
%%%% Entity should be sent in the form of {Pid,{X,Y}}.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec summon_entity(pid(),entity()) -> ok.
summon_entity(Pid, Entity) ->
    gen_server:cast(Pid, {summon_entity, Entity}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Remove an entity from the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec remove_entity(pid(),entity(), atom()) -> ok.
remove_entity(Pid, Entity, Type) ->
    gen_server:cast(Pid, {remove_entity, Entity, Type}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Return the state of the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec get_state(pid()) -> ok.
get_state(Pid) ->
  gen_server:call(Pid,get_state).
 
%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Assign a viewer to the tile.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec set_viewer(pid(),pid()) -> ok.
set_viewer(Pid, ViewerPid) ->
    gen_server:cast(Pid, {set_viewer, ViewerPid}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% Assign the neighbouring tiles viewers.
%%%% @end
%%%%----------------------------------------------------------------------------
-spec set_neighbours(pid(), list()) -> ok.
set_neighbours(Pid, NeighbourPids) ->
    gen_server:cast(Pid, {set_neighbours, NeighbourPids}).

%%%%----------------------------------------------------------------------------
%%%% @doc
%%%% End the tile process.
%%%% @end
%%%%----------------------------------------------------------------------------
terminate(Pid) ->
    gen_server:cast(Pid, terminate).
    

%%%%%%==========================================================================
%%%%%% gen_server Callbacks
%%%%%%==========================================================================

init([Name,X,Y,Size]) ->
    % this registers the name and pid of the process
    erlang:register(Name, self()),
    {ok, #state{xorigin = X, yorigin = Y, xlimit = X+Size-1, ylimit = Y+Size-1, 
                coords = {X,Y,X+Size-1,Y+Size-1,Size}}}.

%%%%-Calls----------------------------------------------------------------------
handle_call(get_geometry,_From,State) ->
    {reply,State#state.coords, State};

handle_call(get_viewer,_From,State) ->
    {reply,State#state.viewer,State};

handle_call(get_neighbours,_From,State) ->
    {reply,State#state.neighbours,State};

handle_call(get_state,_From,State) ->
    {reply,State,State};

handle_call({place_item, {ID,X,Y,Type,Item}}, _From, #state{item_map = ItemMap} = State) ->
    NewMap = maps:put(ID,{X,Y,Type,Item}, ItemMap),
    update_viewers(State#state.neighbours, items, NewMap),
    {reply, ok, State#state{item_map = NewMap}};

handle_call({remove_item, ID}, _From, #state{item_map = ItemMap} = State) ->
    NewMap = maps:remove(ID, ItemMap),
    update_viewers(State#state.neighbours, items, NewMap),
    {reply, ok, State#state{item_map = NewMap}};

%%%% Updates the entities position on the tile.
%%%% Will also deal with a new entitiy being moved onto the tile
handle_call({update_entity, {ID,{OldX,OldY},Type},{NewX,NewY},Velocity},_From, State) when Type == zombie ->
    {TrueX,TrueY} = validmove(OldX,OldY,NewX,NewY,State),
    NewMap = maps:put(ID,{Type,{{TrueX,TrueY},Velocity}},State#state.zombie_map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {reply,{TrueX,TrueY},State#state{zombie_map = NewMap}};
handle_call({update_entity, {ID,{OldX,OldY},Type},{NewX,NewY},Velocity},_From, State) when Type == human ->
    {TrueX,TrueY} = validmove(OldX,OldY,NewX,NewY,State),
    NewMap = maps:put(ID,{Type,{{TrueX,TrueY}, Velocity}},State#state.human_map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {reply,{TrueX,TrueY},State#state{human_map = NewMap}};
    
%%%% pushes a list of obstructed coordinates into the state
handle_call({set_obs_list,New_obs_list},_From,State) ->
    update_viewers(State#state.neighbours, obs_list, New_obs_list),
	{reply,ok,State#state{obs_list=New_obs_list}};

%%% boolean check for obstruction of a pos()
handle_call({check_obs,Pos},_From,State) ->
	{reply,do_check_obs(Pos,State#state.obs_list),State}.

%%%%-Casts----------------------------------------------------------------------

%%%% Handle summon entity, ensure that no entities end up on the same coordinate
%%%% This is only used for the initialisation stage of the application.
%%%% No reply because the environment doesn't care where the new zombie ends up.
handle_cast({summon_entity,{ID,{X,Y},Type}}, #state{zombie_map =Zombie_Map} =State) when Type == zombie ->
    NewMap = maps:put(ID,{Type,{{X,Y},{0,0}}},Zombie_Map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {noreply,State#state{zombie_map = NewMap}};
handle_cast({summon_entity,{ID,{X,Y}, Type}},#state{human_map =Human_Map} =State) when Type == human ->
    NewMap = maps:put(ID,{Type,{{X,Y},{0,0}}},Human_Map),
    update_viewers(State#state.neighbours, Type, NewMap),
    {noreply,State#state{human_map = Human_Map}};

%%%% Handle delete entity calls
handle_cast({remove_entity,ID, zombie},#state{zombie_map =Zombie_Map} =State) ->
    NewMap = maps:remove(ID,Zombie_Map),
    update_viewers(State#state.neighbours, zombie, NewMap),
    {noreply,State#state{zombie_map = NewMap}};
handle_cast({remove_entity,ID, human},#state{human_map =Human_Map} =State) ->
    NewMap = maps:remove(ID,Human_Map),
    update_viewers(State#state.neighbours, human, NewMap),
    {noreply,State#state{human_map = NewMap}};

%%%% Handles setting of tiles viewer
handle_cast({set_viewer, ViewerPid}, State) ->
    {noreply,State#state{viewer = ViewerPid}};

%%%% Add nearby tiles viewers
handle_cast({set_neighbours, NeighbourPids}, State) ->
    {noreply,State#state{neighbours = NeighbourPids}};

%%%% Handle cast to end the system normally
handle_cast(terminate, State) ->
    {stop,normal,State}.

%%%%-Gen-Server-API-------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%==========================================================================
%%%%%% Internal Functions
%%%%%%==========================================================================

update_viewers([], _Type, _EntityMap) ->
    [];
update_viewers([V|Vs], zombie, EntityMap) ->
    viewer:update_zombies(V, {self(), maps:to_list(EntityMap)}),
    update_viewers(Vs, zombie, EntityMap);
update_viewers([V|Vs], human, EntityMap) ->
    viewer:update_humans(V, {self(), maps:to_list(EntityMap)}),
    update_viewers(Vs, human, EntityMap);
update_viewers([V|Vs], items, ItemMap) ->
    viewer:update_items(V, {self(), maps:to_list(ItemMap)}),
    update_viewers(Vs, items, ItemMap);
update_viewers([V|Vs], obs_list, ObsList) ->
    viewer:update_obs(V, {self(), ObsList}),
    update_viewers(Vs, obs_list, ObsList).


%%% check if a move is valid. this is, calculates a valid move if it is not
%% enforce hard speed limit
validmove(X,Y,NewX,NewY,State) when ((NewX - X) > 0) and ((NewX - X) > ?SPEED_HARD_LIMIT) ->
    validmove(X,Y,(X + ?SPEED_HARD_LIMIT),NewY,State);

validmove(X,Y,NewX,NewY,State) when ((NewX - X) <0) and ((NewX - X) < -?SPEED_HARD_LIMIT) ->
    validmove(X,Y,X-?SPEED_HARD_LIMIT,NewY,State);

validmove(X,Y,NewX,NewY,State) when ((NewY - Y) > 0) and ((NewY - Y) > ?SPEED_HARD_LIMIT) ->
    validmove(X,Y,NewX,(Y + ?SPEED_HARD_LIMIT),State);

validmove(X,Y,NewX,NewY,State) when ((NewY - Y) < 0) and ((NewY - Y) < -?SPEED_HARD_LIMIT)->
    validmove(X,Y,NewX,(Y - ?SPEED_HARD_LIMIT),State);    

%% check obstacles and entity bouncing
validmove(X,Y,NewX,NewY,#state{obs_list = ObsList} = State) ->
    case do_check_obs({NewX,NewY},ObsList) of
        % we are obstructed
        true ->
            {ReturnedX, ReturnedY} = reflect_obs(X,Y,ObsList),
            % sanity check, is the returned XY, valid
            case do_check_obs({ReturnedX,ReturnedY},ObsList) of
                true ->
                    {X,Y};
                _ ->
                    % check new position for entitiy clashes
                    validmove_entity(X,Y,ReturnedX,ReturnedY,State)
            end;
        % not obstructed
        _ ->
            {NewX,NewY}
    end.

%% check if bouncing into an entity
validmove_entity(X,Y, NewX, NewY, #state{zombie_map = Zmap, human_map = Hmap, obs_list = Olist}) ->
    case lists:any(fun({_,{{Tx,Ty},_}}) -> {NewX,NewY}=={Tx,Ty} end, maps:values(Zmap) ++ maps:values(Hmap)) of
        % new position will bounce into an entity
        true ->
            % calculate the new position after bouncing off of other entities
            {ReflectedX,ReflectedY} = reflect(X,Y,NewX,NewY),
            {ValidX,ValidY} = reflect_obs(ReflectedX,ReflectedY,Olist),
            % final check, new reflected position is valid
            case do_check_obs({ValidX,ValidY},Olist) of
                true ->
                    % no good, invalid move
                    {X,Y};
                _ ->
                    % all good, you can move
                    {ValidX,ValidY}
            end;
        % move valid, no clash.
        _ -> 
            {NewX,NewY}
    end.
    
%%% reflect away from an entitiy.
reflect(X,Y,TargetX,TargetY)  when {TargetX - X,TargetY - Y} == {0,0} ->
    {X,Y};

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) > 0) and ((TargetY - Y) == 0) ->
    {TargetX-?ENT_REFLECT_RATE,Y+random:uniform(3)-2};

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) < 0) and ((TargetY - Y) == 0) ->
    {TargetX+?ENT_REFLECT_RATE,Y+random:uniform(3)-2};

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) == 0) and ((TargetY - Y) > 0) ->
    {TargetX +random:uniform(3)-2,Y -?ENT_REFLECT_RATE};

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) == 0) and ((TargetY - Y) < 0) ->
    {TargetX +random:uniform(3)-2,Y +?ENT_REFLECT_RATE};

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) > 0) and ((TargetY - Y) > 0) ->
    case random:uniform(3) of
        1 ->
            {TargetX -?ENT_REFLECT_RATE, TargetY};
        2 -> 
            {TargetX -?ENT_REFLECT_RATE, TargetY -?ENT_REFLECT_RATE};
        3 ->
            {TargetX ,TargetY -?ENT_REFLECT_RATE}
    end;

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) < 0) and ((TargetY - Y) < 0) ->
    case random:uniform(3) of
        1 ->
            {TargetX +?ENT_REFLECT_RATE, TargetY};
        2 -> 
            {TargetX +?ENT_REFLECT_RATE, TargetY +?ENT_REFLECT_RATE};
        3 ->
            {TargetX ,TargetY +?ENT_REFLECT_RATE}
    end;

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) < 0) and ((TargetY - Y) > 0) ->
    case random:uniform(3) of
        1 ->
            {TargetX +?ENT_REFLECT_RATE, TargetY};
        2 -> 
            {TargetX +?ENT_REFLECT_RATE, TargetY -?ENT_REFLECT_RATE};
        3 ->
            {TargetX ,TargetY -?ENT_REFLECT_RATE}
    end;

reflect(X,Y,TargetX,TargetY)  when ((TargetX - X) > 0) and ((TargetY - Y) < 0) ->
    case random:uniform(3) of
        1 ->
            {TargetX -?ENT_REFLECT_RATE, TargetY};
        2 -> 
            {TargetX -?ENT_REFLECT_RATE, TargetY +?ENT_REFLECT_RATE};
        3 ->
            {TargetX ,TargetY +?ENT_REFLECT_RATE}
    end.


%%% a function to work out the reflected position when going into an obstruction
reflect_obs(OldX,OldY,Obs_list) ->
    Ways_i_cant_go = 
        % [right,left,down,up]
        [do_check_obs({(OldX)+5,OldY},Obs_list),
        do_check_obs({(OldX)-5,OldY},Obs_list),
        do_check_obs({OldX,(OldY)+5},Obs_list),
        do_check_obs({(OldX),(OldY)-5},Obs_list)],
    reflect_obs({OldX,OldY},Ways_i_cant_go).

% X,Y is obstructed or we wouldn't be here ...

% all obstructed
reflect_obs({OldX,OldY},[true,true,true,true]) -> 
    {OldX,OldY};
% up free
reflect_obs({OldX,OldY},[true,true,true,false]) -> 
    {OldX,OldY-?OBS_REFLECT_RATE};
% down free
reflect_obs({OldX,OldY},[true,true,false,true]) -> 
    {OldX,OldY+?OBS_REFLECT_RATE};
% left free
reflect_obs({OldX,OldY},[true,false,true,true]) -> 
    {OldX-?OBS_REFLECT_RATE,OldY};
%right free
reflect_obs({OldX,OldY},[false,true,true,true]) ->
    {OldX+?OBS_REFLECT_RATE,OldY};
% up and down free
reflect_obs({OldX,OldY},[true,true,false,false]) -> 
    case random:uniform(2) of
        1 ->
            {OldX,OldY+?OBS_REFLECT_RATE};
        2 -> 
            {OldX,OldY-?OBS_REFLECT_RATE}
    end;
% left and up free
reflect_obs({OldX,OldY},[true,false,true,false]) -> 
    case random:uniform(2) of
        1 ->
            {OldX-?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX,OldY-?OBS_REFLECT_RATE}
    end;
% left and down free
reflect_obs({OldX,OldY},[true,false,false,true]) -> 
    case random:uniform(2) of
        1 ->
            {OldX-?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX,OldY+?OBS_REFLECT_RATE}
    end;
% right and up free
reflect_obs({OldX,OldY},[false,true,true,false]) -> 
    case random:uniform(2) of
        1 ->
            {OldX+?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX,OldY-?OBS_REFLECT_RATE}
    end;
% right and down free
reflect_obs({OldX,OldY},[false,true,false,true]) -> 
    case random:uniform(2) of
        1 ->
            {OldX+?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX,OldY+?OBS_REFLECT_RATE}
    end;
% right and left free
reflect_obs({OldX,OldY},[false,false,true,true]) ->
    case random:uniform(2) of
        1 ->
            {OldX+?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX-?OBS_REFLECT_RATE,OldY}
    end;
% left, down and up free
reflect_obs({OldX,OldY},[true,false,false,false]) ->
    case random:uniform(3) of
        1 ->
            {OldX-?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX,OldY+?OBS_REFLECT_RATE};
        3 ->
            {OldX,OldY-?OBS_REFLECT_RATE}
    end;
% right, down and up free
reflect_obs({OldX,OldY},[false,true,false,false]) ->
    case random:uniform(3) of
        1 ->
            {OldX+?OBS_REFLECT_RATE,OldY};
        2 -> 
            {OldX,OldY-?OBS_REFLECT_RATE};
        3 ->
            {OldX,OldY+?OBS_REFLECT_RATE}
    end;
% right, left and up free
reflect_obs({OldX,OldY},[false,false,true,false]) ->
    case random:uniform(3) of
        1 ->
            {OldX,OldY-?OBS_REFLECT_RATE};
        2 -> 
            {OldX-?OBS_REFLECT_RATE,OldY};
        3 ->
            {OldX+?OBS_REFLECT_RATE,OldY}
    end;
% right, left and down free
reflect_obs({OldX,OldY},[false,false,false,true]) ->
    case random:uniform(3) of
        1 ->
            {OldX,OldY+?OBS_REFLECT_RATE};
        2 ->   
            {OldX-?OBS_REFLECT_RATE,OldY};
        3 ->
            {OldX+?OBS_REFLECT_RATE,OldY}
    end;
% all free!
reflect_obs({OldX,OldY},[false,false,false,false]) ->
    case random:uniform(4) of
        1 ->
            {OldX+?OBS_REFLECT_RATE,OldY};
        2 ->   
            {OldX,OldY+?OBS_REFLECT_RATE};
        3 ->
            {OldX,OldY-?OBS_REFLECT_RATE};
        4 ->
            {OldX-?OBS_REFLECT_RATE,OldY}
    end.

%%%==============
%%% This is called to check if a coordinate pair is obstructed
%%% =============
-spec do_check_obs(pos(),list()) -> boolean().
do_check_obs({X,Y},Obs_list) ->
	lists:any(fun(C) -> C=={X div 5,Y div 5} end,Obs_list).
	
%%%%-Notes----------------------------------------------------------------------

% observer:start().
