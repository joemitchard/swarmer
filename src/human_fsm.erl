-module(human_fsm).
-author("Joe Mitchard jm710").

-define(SIGHT,99).
-define(PERSONAL_SPACE, 3).

%Variables for boids.
-define(SPEED_LIMIT,5).
-define(TIRED_SPEED_LIMIT,2).
-define(HUNGRY_SPEED_LIMIT,4).
-define(SUPER_EFFECT, 0.3).
-define(FLOCKING_EFFECT,0.5).
-define(VELOCITY_EFFECT,0.5).
-define(COHESION_EFFECT,0.2).
-define(LONGEST_SEARCH_DISTANCE,10).

% Behaviour Parameters
-define(INITIAL_HUNGER,100).
-define(INITIAL_ENERGY,100 ).
-define(HUNGRY_LEVEL, 25).
-define(TIRED_LEVEL, 25).

-behaviour(gen_fsm).

%%% gen_fsm functions
-export([code_change/4,handle_event/3,handle_sync_event/4,
         handle_info/3,init/1,terminate/3]).

%%% system functions
-export([start_link/8,run/2,initial/2,start/1,pause/2]).

%%% API exports
-export([get_state/1, pause/1, unpause/1,zombify/1]).

-record(state, {id,
                % localisation
                tile, viewer, viewerStr,
                tile_size, num_columns, num_rows,
                % movement control
                x_velocity, y_velocity,
                x,y,
                % timing and state
                timeout, type, paused_state,
                % lists to maintain local awareness
                z_list, h_list, i_list, memory_list,
                % elements for behavioural control
                hunger_state, hunger, energy,
                memory_map = maps:new(), path,
                obs_list}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Timeout) ->
    gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,
                                Timeout],[]).

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

start(Pid) ->
    gen_fsm:send_event(Pid,start).

pause(Pid) ->
    gen_fsm:send_all_state_event(Pid, pause).

unpause(Pid) ->
    gen_fsm:send_event(Pid,unpause).

zombify(Pid) ->
    gen_fsm:send_all_state_event(Pid, zombify).

get_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_state).

init([X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Timeout]) ->
    random:seed(erlang:monotonic_time()),
    tile:summon_entity(Tile,{self(),{X,Y}, human}),
    {ok,initial,#state{id = list_to_binary(pid_to_list(self())),
                       tile = Tile,viewer = Viewer, x = X, y = Y,
                       timeout=Timeout,type =human,
                       viewerStr = list_to_binary(pid_to_list(Viewer)),
                       tile_size = TileSize, num_columns = NumColumns,
                       num_rows = NumRows,
                       x_velocity = 0, y_velocity = 0,
                       hunger = ?INITIAL_HUNGER, energy = ?INITIAL_ENERGY,
                       hunger_state = not_hungry,
                       obs_list = []}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.timeout, check_pos),
    {next_state,run,State}.

%%% failsafe test to ensure the entity is still in a valid position
run(check_pos,#state{x=X, y=Y, obs_list = Olist, tile = Tile, type = Type} = State) ->
    % hard check to see if valid position
    case check_valid_pos({X,Y},Olist) of
        true ->
            % physics stopped existing, I'm in a wall.
            tile:remove_entity(Tile, self(), Type),
            {stop, shutdown, State};
            % all good!
        _ ->
            gen_fsm:send_event(self(),move),
            {next_state,run,State}
    end;

run(move,#state{    x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type,
                    x_velocity = X_Velocity, y_velocity = Y_Velocity,
                    viewer = Viewer,
                    hunger = Hunger, energy = Energy,
                    memory_map = MemoryMap,
                    path = Path} = State) ->

    Olist = viewer:get_obs(Viewer),

    % Build a list of nearby zombies
    Zlist = build_zombie_list(Viewer, X, Y,Olist),

    % Build a list of nearby humans
    Hlist = build_human_list(Viewer, X, Y,Olist),

    % Build a list of nearby items and store them to memory
    Ilist = viewer:get_items(Viewer),

        I_Sight_List = lists:filter(
                                fun(
                                    {_,{IX,IY,_,_}}) ->
                                      los:findline(X,Y,IX,IY,Olist)
                                end,Ilist),

    NewMemoryMap = build_memory(I_Sight_List, MemoryMap),

    %Olist = viewer:get_obs(Viewer),

    Zlist_Json = swarm_libs:jsonify_list(Zlist),
    Hlist_Json = swarm_libs:jsonify_list(Hlist),

    % creates a new value for hunger and food, showing the humans getting
    % hungry over time
    {NewHunger, NewEnergy, NewHungerState} = calc_new_hunger_levels(Hunger,Energy),

    NearestItem = get_nearest_item(Ilist,{X,Y}),

    % For this, I need to pass the path to make choice, and add a new condition to
    % continue on the path if there is one, but no other cases.
    {{BoidsX, BoidsY}, NewPath, EatenHunger, EatenEnergy} = case NewHungerState of
        tired ->
            % need to search for food, boids a little, but also limit speed
            MemoryList = maps:keys(NewMemoryMap),
            case calc_new_hungry_xy(Hlist,Zlist,NearestItem,NewHungerState,
                                X, Y, Olist, MemoryList, Path, State) of
                {BX,BY,NewP,eaten} ->
                    {{BX,BY},NewP,?INITIAL_HUNGER,?INITIAL_ENERGY};
                {BX,BY,NewP} ->
                    {{BX,BY},NewP,NewHunger,NewEnergy}
            end;
        very_hungry ->
            % need to search for food, boids a little, but also limit speed
            MemoryList = maps:keys(NewMemoryMap),
            case calc_new_hungry_xy(Hlist,Zlist,NearestItem,NewHungerState,
                                X, Y, Olist, MemoryList, Path, State) of
                {BX,BY,NewP,eaten} ->
                    {{BX,BY},NewP,?INITIAL_HUNGER,?INITIAL_ENERGY};
                {BX,BY,NewP} ->
                    {{BX,BY},NewP,NewHunger,NewEnergy}
            end;
        hungry ->
            % search for food, but also boids
            {make_choice(Hlist,Zlist, NearestItem, NewHungerState, Path, State),[],NewHunger,NewEnergy};
        not_hungry ->
            % save any food you find to a map, boids as normal
            {make_choice(Hlist,Zlist, NearestItem, NewHungerState, Path, State),[],NewHunger,NewEnergy}
    end,

    New_X_Velocity = X_Velocity + BoidsX,
    New_Y_Velocity = Y_Velocity + BoidsY,

    {Limited_X_Velocity,Limited_Y_Velocity} = case NewHungerState of
        tired ->
            % need to limit speed drastically
            boids_functions:limit_speed(?TIRED_SPEED_LIMIT,X,Y,New_X_Velocity,New_Y_Velocity);
        very_hungry ->
            % need to search for food, boids a little, but also limit speed
            boids_functions:limit_speed(?HUNGRY_SPEED_LIMIT,X,Y,New_X_Velocity,New_Y_Velocity);
        _ ->
            % need to search for food, boids a little, but also limit speed
            boids_functions:limit_speed(?SPEED_LIMIT,X,Y,New_X_Velocity,New_Y_Velocity)
    end,

    TargetX = round(X + Limited_X_Velocity),
    TargetY = round(Y + Limited_Y_Velocity),
    {NewX,NewY,ObsVelX,ObsVelY} = obstructed(Olist,X,Y,TargetX,TargetY,Limited_X_Velocity,Limited_Y_Velocity),

    case (NewX < 0) or (NewY < 0) or (NewX > NumColumns * (TileSize-1)) or (NewY > NumRows * (TileSize-1)) of
        true -> % We are off the screen!
            {stop, shutdown, State};
        false ->
            NewTile =
            % This calculates if the human is still in it's initial tile
            case {trunc(X) div TileSize, trunc(NewX) div TileSize, trunc(Y) div TileSize, trunc(NewY) div TileSize} of
                {XTile, XTile, YTile, YTile} ->
                    % In same tile
                    Tile;
                {_, NewXTile, _, NewYTile} ->
                    % Left the tile, find new tile and remove self from old one.
                    tile:remove_entity(Tile, self(), Type),
                    % Generates an atom based on location, this will be a name of a tile registered to Erlang
                    list_to_atom("tile" ++  "X" ++ integer_to_list(NewXTile) ++  "Y" ++ integer_to_list(NewYTile))
            end,

            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y}, Type},{NewX, NewY},{X_Velocity, Y_Velocity}),
            gen_fsm:send_event_after(State#state.timeout, check_pos),

            {next_state,run,State#state{x=ReturnedX,y=ReturnedY,
                                        tile = NewTile,
                                        z_list = Zlist_Json, h_list = Hlist_Json,
                                        x_velocity = ObsVelX,
                                        y_velocity = ObsVelY,
                                        hunger = EatenHunger, energy = EatenEnergy,
                                        hunger_state = NewHungerState,
                                        memory_map = NewMemoryMap,
                                        path = NewPath,
                                        obs_list = Olist}}
    end.

%%%%%%==========================================================================
%%%%%% Event and Sync Functions
%%%%%%==========================================================================

pause(check_pos, State) ->
    %% If we get a move event start the timer again but don't actually move
    %% Ensure we will move after unpause.
    gen_fsm:send_event_after(State#state.timeout, check_pos),
    {next_state, pause, State};
pause(unpause, #state{paused_state = PausedState} = State) ->
    {next_state,PausedState,State}.

terminate(_,_StateName, #state{tile = Tile, type = Type} = _StateData) ->
    tile:remove_entity(Tile, self(), Type),
    ok.

code_change(_,StateName,StateData,_) ->
    {ok,StateName,StateData}.

handle_info(_,StateName,StateData)->
    {ok,StateName,StateData}.

handle_event(pause, StateName, StateData) ->
    {next_state,pause,StateData#state{paused_state = StateName}};

handle_event(zombify, _StateName, #state{x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile,
                    viewer = Viewer, timeout = Timeout} = StateData) ->
    {ok,Zombie}=supervisor:start_child(zombie_sup,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Timeout,0]),
    zombie_fsm:start(Zombie),
    {stop, shutdown, StateData}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    PropList = record_to_proplist(StateData),
    % take the pid out of the report for JSX
    PropListNoViewer = proplists:delete(viewer,PropList),
    % take the MemoryMap out of the report for JSX
    PropListNoMemory = proplists:delete(memory_map,PropListNoViewer),
    PropListNoObs = proplists:delete(obs_list,PropListNoMemory),
    {reply, {ok,PropListNoObs}, StateName,StateData}.

record_to_proplist(#state{} = Record) ->
    lists:zip(record_info(fields, state), tl(tuple_to_list(Record))).


%%%%%%==========================================================================
%%%%%% Boids Functions
%%%%%%==========================================================================
% Make choice is called with ->
%          (HumanList, ZombieList, NearestItem, HungerState, State)
make_choice([],[],_NearestItem, _HungerState, _Path, _State) ->
    % Nothing around, wander aimlessly
    case random:uniform(9) of
        1->
            {0,0};
        2->
            {0,-1};
        3->
            {0,1};
        4->
            {-1,0};
        5->
            {-1,-1};
        6->
            {-1,1};
        7->
            {1,0};
        8->
            {1,-1};
        9->
            {1,1}
    end;

%===========================Collision Avoidance================================%
make_choice([{Dist, {_,{_,{{HeadX,HeadY},{_,_}}}}}|_],_,_,_, _Path, #state{x=X,y=Y}) when Dist < ?PERSONAL_SPACE ->
    boids_functions:collision_avoidance(X, Y, HeadX, HeadY,?COHESION_EFFECT);

%=============================Super Repulsor====================================%
make_choice(_,[{_Dist, {_,{_,{{ZomX,ZomY},{_,_}}}}}|_],_, _, _Path, #state{x=X,y=Y}) ->
    boids_functions:super_repulsor(X,Y,ZomX,ZomY,?SUPER_EFFECT);

%===============================Flocking========================================%
make_choice(Hlist,_,_NearestItem, not_hungry, _Path, #state{x=X,y=Y,x_velocity = XVel, y_velocity = YVel}) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,X,Y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,XVel,YVel,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy)};

make_choice(Hlist,_,_NearestItem, hungry, _Path, #state{x=X,y=Y,x_velocity = XVel, y_velocity = YVel}) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,X,Y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,XVel,YVel,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy)};

%============================Hungry - Local Item================================%
% There is no zombie, I do have a path!
make_choice(_,[],_, very_hungry, Path, _State) when length(Path) >= 1 ->
    [Next|Rest] = Path,
    {BX,BY} = Next,
    {BX,BY,Rest};

make_choice(_,[],_, tired, Path, _State) when length(Path) >= 1 ->
    [Next|Rest] = Path,
    {BX,BY} = Next,
    {BX,BY,Rest};

% There is no zombie, I have no path, move towards food blindly!
make_choice(_,[],{ItemId,{ItemX,ItemY,food,_}}, very_hungry, _Path, #state{x=X, y=Y}) ->
    case swarm_libs:pyth(X,Y,ItemX,ItemY) of
        Value when Value =< 2 ->
            Item = supplies:picked_up(ItemId),
            case Item of
                ok ->
                    {X,Y,eaten};
                _ ->
                    {X,Y}
            end;
        _ ->
            boids_functions:super_attractor(X,Y,ItemX,ItemY,?SUPER_EFFECT)
    end;

make_choice(_,[],{ItemId,{ItemX,ItemY,food,_Name}}, tired, _Path, #state{x=X, y=Y}) ->
    case swarm_libs:pyth(X,Y,ItemX,ItemY) of
        Value when Value =< 2 ->
            Item = supplies:picked_up(ItemId),
            case Item of
                ok ->
                    {X,Y,eaten};
                _ ->
                    {X,Y}
            end;
        _ ->
            boids_functions:super_attractor(X,Y,ItemX,ItemY,?SUPER_EFFECT)
    end;

% Got some humans, but no food
make_choice(Hlist,_,nothing_found, very_hungry, _Path, #state{x=X,y=Y,x_velocity = XVel, y_velocity = YVel}) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,X,Y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,XVel,YVel,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy),nothing_found};

make_choice(Hlist,_,nothing_found, tired, _Path, #state{x=X,y=Y,x_velocity = XVel, y_velocity = YVel}) ->
    {Fx,Fy} = boids_functions:flocking(Hlist,X,Y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Hlist,XVel,YVel,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy),nothing_found}.

%%%%%%==========================================================================
%%%%%% Functions for Boids Functions
%%%%%%==========================================================================

%%% A function to find the closest item to the human
get_nearest_item([],_) ->
    nothing_found;
get_nearest_item([I|Is], {HumanX, HumanY}) ->
    get_nearest_item(Is, {HumanX, HumanY}, I).

%%% Find the nearest item in sight
get_nearest_item([], _, NearestItem) ->
    NearestItem;
get_nearest_item([{ID,{X,Y,Type,Item}}|Is],{HumanX, HumanY}, {NID,{NearestX,NearestY,NType,NItem}}) ->
    BestItem = swarm_libs:pyth(NearestX, NearestY, HumanX, HumanY),
    case swarm_libs:pyth(X, Y, HumanX, HumanY) of
        Value when Value < BestItem ->
            get_nearest_item(Is, {HumanX, HumanY}, {ID, {X,Y,Type,Item}});
        _ ->
            get_nearest_item(Is, {HumanX, HumanY}, {NID,{NearestX,NearestY,NType,NItem}})
    end.

%%% Ask astar for a path to an item, avoiding obstacles
pathfind_to_item([Head|Rest], CurrentPos, ObsList) ->
    NearestMemoryItem = nearest_memory_item(Rest, Head, CurrentPos),
    Distance = astar:dist_between(CurrentPos,NearestMemoryItem),
    pathfind_to_item(NearestMemoryItem,CurrentPos,Distance,ObsList).

pathfind_to_item(NearestItem,CurrentPos,Distance,ObsList) when Distance =< ?LONGEST_SEARCH_DISTANCE ->
    astar:astar(CurrentPos,NearestItem, ObsList);
pathfind_to_item(_NearestItem,_CurrentPos,_Distance,_ObsList) ->
    too_far_away.

%%% Find the nearest item from memory
nearest_memory_item([], Nearest, _CurrentPos) ->
    Nearest;
nearest_memory_item([Head|Rest], Nearest, CurrentPos) ->
    NearestDist = astar:dist_between(Nearest,CurrentPos),
    case astar:dist_between(Head,CurrentPos) of
        Value when Value < NearestDist ->
            % Head of the list is closer than current best
            nearest_memory_item(Rest,Head,CurrentPos);
        _ ->
            % Current best is still best
            nearest_memory_item(Rest,Nearest,CurrentPos)
    end.

%%% Check if my next position is obstructed
obstructed([],_X,_Y,NewX,NewY,VelX,VelY) ->
    {NewX,NewY,VelX,VelY};
obstructed(Olist,X,Y,NewX,NewY,VelX,VelY) ->
    Member = lists:any(fun({A,B}) -> NewY div 5 == B andalso NewX div 5 == A end,Olist),
    case Member of
        true->
            swarm_libs:obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY);
        false->
            {NewX,NewY,VelX,VelY}
    end.



%%% Calculate the new levels for hunger,energy
%%% Also work out if the hunger state has changed
calc_new_hunger_levels(Hunger,Energy) ->
    case Hunger of
        HValue when HValue =< 0 ->
            case Energy of
                EVAlue when EVAlue =< ?TIRED_LEVEL ->
                    {Hunger,Energy, tired};
                _ ->
                    {Hunger, Energy-1, very_hungry}
            end;
        HValue when HValue =< ?HUNGRY_LEVEL ->
            {Hunger-1, Energy-1, hungry};
        _ ->
            {Hunger-1, Energy, not_hungry}
    end.

%%% A function to calculate the new X,Y and path for the Human.
calc_new_hungry_xy(Hlist, Zlist, NearestItem, NewHungerState, X, Y, MemoryList, Olist, Path, State) ->
    case make_choice(Hlist,Zlist, NearestItem, NewHungerState, Path, State) of
        {BX,BY} ->
            % Local food! Go forth hungry human!
            {BX,BY,Path};
        {BX,BY,nothing_found} when length(MemoryList) =:= 0 ->
            % No local food, doesn't remember any food...
            % Wander around until you starve poor human!
            {BX,BY,Path};
        {BX,BY,nothing_found} ->
            % No local food, does remember food however...
            % Pathfind to some food you remember
            NewPath = pathfind_to_item(MemoryList, {X,Y}, Olist),
            case NewPath of
                too_far_away ->
                    {BX,BY,Path};
                _ ->
                    [{PathX,PathY}|Rest] = NewPath,
                    {PathX,PathY,Rest}
            end;
        {BX,BY,eaten} ->
            {BX,BY,Path,eaten};
        {BX,BY,RestOfPath} ->
            % Pathfinding towards food...
            {BX,BY,RestOfPath}
    end.

%%%%%%==========================================================================
%%%%%% List Organisation and Setup Functions
%%%%%%==========================================================================

%%% Build a list of local zombie entities that are in sight
build_zombie_list(Viewer, X, Y,Olist) ->
    ZombieList = viewer:get_zombies(Viewer),

    Z_DistanceList = lists:map(fun(
                                {ZomPid,{ZType,{{ZX,ZY},{ZX_Velocity,ZY_Velocity}}}}) ->
                                    {abs(swarm_libs:pyth(X,Y,ZX,ZY)),
                                    {ZomPid,{ZType,{{ZX,ZY},
                                    {ZX_Velocity,ZY_Velocity}}}}}
                                end,ZombieList),

    Z_FilteredList = lists:filter(
                                fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
                                    Dist =< ?SIGHT
                                end,Z_DistanceList),

        Z_Sight_List = lists:filter(
                                fun({_,
                                    {_,{_,{{ZX,ZY},
                                    {_,_}}}}}) ->
                                      los:findline(X,Y,ZX,ZY,Olist)
                                      end,Z_FilteredList),

    Zlist = lists:keysort(1,Z_Sight_List),
    %return
    Zlist.

%%% Build a list of local zombie entities that are in sight
build_human_list(Viewer, X, Y,Olist) ->
    HumanList = viewer:get_humans(Viewer),
    NoSelfList = lists:keydelete(self(),1,HumanList),

    H_DistanceList = lists:map(fun(
                                {Hpid,{human,{{HX,HY},{HXV,HYV}}}}) ->
                                    {abs(swarm_libs:pyth(X,Y,HX,HY)),
                                    {Hpid,{human,{{HX,HY},
                                    {HXV,HYV}}}}}
                            end,NoSelfList),

    H_FilteredList = lists:filter(
                                fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
                                    Dist =< ?SIGHT
                                end,H_DistanceList),

        H_Sight_List = lists:filter(
                                fun({_,
                                    {_,{_,{{HX,HY},
                                    {_,_}}}}}) ->
                                      los:findline(X,Y,HX,HY,Olist)
                                      end,H_FilteredList),

    Hlist = lists:keysort(1,H_Sight_List),
    %return
    Hlist.

%%% Build memory map for items
build_memory([], Map) ->
    Map;
build_memory([{Pid,{X,Y,Type,Name}}|Rest], Map) ->
    NewMap = maps:put({X,Y}, {Pid,Type,Name}, Map),
    build_memory(Rest, NewMap).

check_valid_pos({X,Y},Obs_list) ->
    lists:any(fun(C) -> C=={X div 5,Y div 5} end,Obs_list).
