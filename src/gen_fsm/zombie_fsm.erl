-module(zombie_fsm).
-author("Robert Hales rsjh3@kent.ac.uk").

-define(SIGHT,75).
-define(PERSONAL_SPACE, 3).
-define(ENERGY_INIT,100).
%Variables for boids.
-define(LIMIT,3).
-define(LIMIT_SLOW,1).
-define(SUPER_EFFECT, 0.3).
-define(FLOCKING_EFFECT,0.05).
-define(VELOCITY_EFFECT,0.5).
-define(COHESION_EFFECT,0.2).

-behaviour(gen_fsm).

%gen_fsm implementation
-export([code_change/4,handle_event/3,handle_sync_event/4,
		 handle_info/3,init/1,terminate/3]).

-export([build_human_list/4, build_zombie_list/4]).

-export([start_link/9,aimless/2,initial/2,
         start/1,pause/2,get_surroundings/2,
		 find_visible/2,find_visible/3]).

%API
-export([get_state/1, pause/1, unpause/1]).

-record(state, {id,
                tile,tile_size,
                num_columns,num_rows,
                viewer,viewerStr,
                speed,
                x,y,type,
                paused_state,
                x_velocity,y_velocity,
                z_list,h_list,
                energy,energy_state}).

start_link(X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing) ->
	gen_fsm:start_link(?MODULE,[X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,Bearing],[]).


%%% API!
start(Pid) ->
    gen_fsm:send_event(Pid,start).

pause(Pid) ->
    gen_fsm:send_all_state_event(Pid, pause).

unpause(Pid) ->
    gen_fsm:send_event(Pid,unpause).

get_state(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, get_state).

init([X,Y,Tile,TileSize,NumColumns,NumRows,Viewer,Speed,_Bearing]) ->
	random:seed(erlang:monotonic_time()),
    tile:summon_entity(Tile,{self(),{X,Y}, zombie}),
	{ok,initial,#state{id = list_to_binary(pid_to_list(self())),
                       tile = Tile,viewer = Viewer, x = X, y = Y,speed=Speed,
                       type =zombie,
                       viewerStr = list_to_binary(pid_to_list(Viewer)),
                       tile_size = TileSize, num_columns = NumColumns,
                       num_rows = NumRows,
                       x_velocity = 0, y_velocity = 0,
                       energy = ?ENERGY_INIT, energy_state = ok}}.

%%%%%%==========================================================================
%%%%%% State Machine
%%%%%%==========================================================================

initial(start,State) ->
    gen_fsm:send_event_after(State#state.speed, move),
	{next_state,aimless,State}.

aimless(move,#state{speed = Speed, x = X, y = Y, tile_size = TileSize,
                    num_columns = NumColumns, num_rows = NumRows,
                    tile = Tile, type = Type,
                    x_velocity = X_Velocity, y_velocity = Y_Velocity,
                    energy = Energy, energy_state = EnergyState} = State) ->

    NewViewer = tile:get_viewer(Tile),
		Olist = viewer:get_obs(NewViewer),

		Zlist = build_zombie_list(NewViewer, X, Y, Olist),
		Hlist = build_human_list(NewViewer, X, Y, Olist),

    %Olist = viewer:get_obs(NewViewer),
    Zlist_Json = swarm_libs:jsonify_list(Zlist),
    Hlist_Json = swarm_libs:jsonify_list(Hlist),

    % calculate new energy levls
    {NewEnergy, NewEnergyState} = case Energy of
        Value when Value =:= 0 ->
            {Energy,slowed};
        _ ->
            {Energy-1,EnergyState}
    end,

    {BoidsX,BoidsY,FinalEnergy} =
    case make_choice(Zlist,Hlist,State) of
        {Bx,By,eaten} ->
            {Bx,By,?ENERGY_INIT};
        {Bx,By} ->
            {Bx,By,NewEnergy}
    end,

    New_X_Velocity = X_Velocity + BoidsX,
    New_Y_Velocity = Y_Velocity + BoidsY,

    % slow entities down when they are out of energy
    {Limited_X_Velocity,Limited_Y_Velocity} = case NewEnergyState of
        slowed ->
            boids_functions:limit_speed(?LIMIT_SLOW,X,Y,New_X_Velocity,New_Y_Velocity);
        _ ->
            boids_functions:limit_speed(?LIMIT,X,Y,New_X_Velocity,New_Y_Velocity)
    end,

    TargetX = round(X + Limited_X_Velocity),
    TargetY = round(Y + Limited_Y_Velocity),

    {NewX,NewY,ObsXVel,ObsYVel} = swarm_libs:obstructed(Olist,X,Y,
																													TargetX,
																													TargetY,
																													Limited_X_Velocity,
																													Limited_Y_Velocity),

    case (NewX < 0) or (NewY < 0) or (NewX > NumColumns * (TileSize-1)) or (NewY > NumRows * (TileSize-1)) of
        true -> % We are off the screen!
            {stop, shutdown, State};
        false ->
            NewTile =
            % This calculates if the human is still in it's initial tile
            case {trunc(X) div TileSize, trunc(NewX) div TileSize, trunc(Y) div TileSize, trunc(NewY) div TileSize} of
                {XTile, XTile, YTile, YTile} -> % In same tile
                    Tile;
                {_, NewXTile, _, NewYTile} ->
                    tile:remove_entity(Tile, self(), Type),
                    list_to_atom("tile" ++  "X" ++ integer_to_list(NewXTile) ++  "Y" ++ integer_to_list(NewYTile))
            end,

            {ReturnedX,ReturnedY} = tile:update_entity(NewTile,{self(),{X,Y},Type},{NewX, NewY}, {New_X_Velocity, New_Y_Velocity}),

            gen_fsm:send_event_after(Speed, move),

            {next_state,aimless,State#state{x=ReturnedX,y=ReturnedY,
                                            tile = NewTile, z_list = Zlist_Json, h_list = Hlist_Json,
                                            x_velocity = ObsXVel,y_velocity = ObsYVel, viewer = NewViewer,
                                            energy = FinalEnergy, energy_state = NewEnergyState}}
    end.

pause(move, State) ->
    %% If we get a move event start the timer again but don't actually move
    %% Ensure we will move after unpause.
    gen_fsm:send_event_after(State#state.speed, move),
    {next_state, pause, State};
pause(unpause, #state{paused_state = PausedState} = State) ->
	{next_state,PausedState,State}.

%Events for fsm.
get_surroundings(_Pid,#state{viewer=Viewer} = State) ->
	Map = viewer:get_population(Viewer),
		case maps:size(Map) of
			0-> [];
			_ -> Surroundings = maps:values(Map),
					find_visible(Surroundings,State)
		end.

find_visible(All,State) ->
	Visible = [],
	find_visible(All,State,Visible).
find_visible([],_State,Visible) ->
	Visible;
find_visible([[{Pid,{_Otherx,_Othery}}]|_Tail],_State,_Visible) ->
	error_logger:error_report(Pid),
	[].

%stuff for gen_fsm.
terminate(_,_StateName, #state{tile = Tile, type = Type} = _StateData) ->
    tile:remove_entity(Tile, self(), Type),
	ok.
code_change(_,StateName,StateData,_) ->
	{ok,StateName,StateData}.
handle_info(_,StateName,StateData)->
	{ok,StateName,StateData}.

handle_event(pause, StateName, StateData) ->
    {next_state,pause,StateData#state{paused_state = StateName}}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    PropList = record_to_proplist(StateData),
    PropListJson = proplists:delete(viewer,PropList),
    {reply, {ok,PropListJson}, StateName,StateData}.

record_to_proplist(#state{} = Record) ->
    lists:zip(record_info(fields, state), tl(tuple_to_list(Record))).

make_choice([],[],_State) ->
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

make_choice(_,[{Dist, {Pid,{_,{{_,_},{_,_}}}}}|_Hlist],_State) when Dist < ?PERSONAL_SPACE ->
%    KILL HUMAN;
    case random:uniform(3) of
        1 ->
            {0,0};
        _ ->
            human_fsm:zombify(Pid),
            {0,0,eaten}
    end;

make_choice([{Dist, {_,{_,{{HeadX,HeadY},{_Head_X_Vel,_Head_Y_Vel}}}}}|_Zlist],_,State) when Dist < ?PERSONAL_SPACE ->
    boids_functions:collision_avoidance(State#state.x, State#state.y, HeadX, HeadY,?COHESION_EFFECT);

make_choice(_,[{_Dist, {_,{_,{{HeadX,HeadY},{_Head_X_Vel,_Head_Y_Vel}}}}}|_Hlist],State) ->
    boids_functions:super_attractor(State#state.x,State#state.y,HeadX,HeadY,?SUPER_EFFECT);

make_choice(Zlist,_, State) ->
    {Fx,Fy} = boids_functions:flocking(Zlist,State#state.x,State#state.y,?FLOCKING_EFFECT),
    {Vx,Vy} = boids_functions:velocity(Zlist,State#state.x_velocity,State#state.y_velocity,?VELOCITY_EFFECT),
    {(Fx+Vx),(Fy+Vy)}.

build_human_list(NewViewer, X, Y, Olist) ->
	HumanList = viewer:get_humans(NewViewer),
	Hlist = swarm_libs:build_entity_list(HumanList, X, Y, Olist,?SIGHT),
	Hlist.

build_zombie_list(NewViewer, X, Y, Olist) ->
	ZombieList = viewer:get_zombies(NewViewer),
	NoSelfList = lists:keydelete(self(),1,ZombieList),
	Zlist = swarm_libs:build_entity_list(NoSelfList, X, Y, Olist,?SIGHT),
	Zlist.
