-module(swarm_libs).
-export([round/2, pyth/4, cleanup/6, restart_proc/2, jsonify_list/1,
					obstructedmove/7, obstructed/7, build_entity_list/5]).

-export([do_start_entities/0, do_pause_entities/0, do_unpause_entities/0,
				 do_action_entities_type/2, apply_to_all__humans/1, apply_to_all_zombies/1,
				 apply_to_all_entities/1,
				 get_entities_list/0, get_humans_list/0, get_zombies_list/0, get_supplies_list/0]).

round(Num,Precision) ->
	P = math:pow(10,Precision),
	round(Num * P) / P.

pyth(X,Y,X2,Y2)->
	math:sqrt(math:pow((X-X2),2)+math:pow((Y-Y2),2)).

cleanup(SwarmSup, ZombieSup, HumanSup, SuppliesSup, TileSup, ViewerSup) ->
	%Kill entities
	supervisor:terminate_child(SwarmSup, ZombieSup),
	supervisor:terminate_child(SwarmSup, HumanSup),
	supervisor:terminate_child(SwarmSup, SuppliesSup),
	%Restart entities
	supervisor:restart_child(SwarmSup, ZombieSup),
	supervisor:restart_child(SwarmSup, HumanSup),
	supervisor:restart_child(SwarmSup, SuppliesSup),
	%Kill tiles
	supervisor:terminate_child(SwarmSup, TileSup),
	supervisor:restart_child(SwarmSup, TileSup),
	%Kill viewers
	supervisor:terminate_child(SwarmSup, ViewerSup),
	supervisor:restart_child(SwarmSup, ViewerSup),
	ok.

restart_proc(SwarmSup, Sup) ->
	supervisor:terminate_child(SwarmSup, Sup),
	supervisor:restart_child(SwarmSup, Sup).

do_start_entities() ->
    apply_to_all_entities(start).

do_pause_entities() ->
    apply_to_all_entities(pause).

do_unpause_entities() ->
    apply_to_all_entities(unpause).

do_action_entities_type(Action,Type) ->
    case Action of
      pause ->
        case Type of
          humans ->
            apply_to_all__humans(pause);
          zombies ->
            apply_to_all_zombies(pause)
        end;
      unpause ->
        case Type of
          humans ->
            apply_to_all__humans(unpause);
          zombies ->
            apply_to_all_zombies(unpause)
        end
    end.


apply_to_all_entities(Fun) ->
    lists:foreach(
        fun({_Id, Pid, _Type, [Module]}) ->
            Module:Fun(Pid)
        end, get_entities_list()).

apply_to_all_zombies(Fun) ->
    lists:foreach(
        fun({_Id, Pid, _Type, [Module]}) ->
            Module:Fun(Pid)
        end, get_zombies_list()).

apply_to_all__humans(Fun) ->
    lists:foreach(
        fun({_Id, Pid, _Type, [Module]}) ->
            Module:Fun(Pid)
        end, get_humans_list()).

get_entities_list() ->
  get_zombies_list() ++ get_humans_list().

get_zombies_list() ->
  supervisor:which_children(zombie_sup).

get_humans_list() ->
  supervisor:which_children(human_sup).

get_supplies_list() ->
  supervisor:which_children(supplies_sup).

%%% Obstructions on corners.
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX > 0) and (VelY > 0)->
    {X-1,Y-1,-1,-1};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX > 0) and (VelY < 0)->
    {X-1,Y+1,-1,1};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX < 0) and (VelY > 0)->
    {X+1,Y-1,1,-1};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX < 0) and (VelY < 0)->
    {X+1,Y+1,1,1};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX > 0) and (VelY == 0)->
    {X-1,Y,-1,0};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX < 0) and (VelY == 0)->
    {X+1,Y,1,0};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX == 0) and (VelY > 0)->
    {X,Y-1,0,-1};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX == 0) and (VelY < 0)->
    {X,Y+1,0,1};
obstructedmove(_Olist,X,Y,NewX,NewY,VelX,VelY) when (X == NewX) and (Y == NewY) and (VelX == 0) and (VelY == 0)->
    {X,Y,0,0};

%%% Obstructions on Y axis.
obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY) when ((abs(NewX-X)) >= (abs(NewY-Y))) and (VelY > 0)->
    Member = lists:any(fun({A,B}) -> Y div 5 == B andalso NewX div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,X,NewY,VelX,VelY);
        false->
            {NewX,Y-1,VelX,-1}
    end;
obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY) when ((abs(NewX-X)) >= (abs(NewY-Y))) and (VelY < 0)->
    Member = lists:any(fun({A,B}) -> Y div 5 == B andalso NewX div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,X,NewY,VelX,VelY);
        false->
            {NewX,Y+1,VelX,1}
    end;
obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY) when ((abs(NewX-X)) >= (abs(NewY-Y))) and (VelY == 0)->
    Member = lists:any(fun({A,B}) -> Y div 5 == B andalso NewX div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,X,NewY,VelX,VelY);
        false->
            {NewX,Y,VelX,0}
    end;

%%% Obstructions on X axis.
obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY) when ((abs(NewY-Y)) > (abs(NewX-X))) and (VelX > 0)->
    Member = lists:any(fun({A,B}) -> NewY div 5 == B andalso X div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,NewX,Y,VelX,VelY);
        false->
            {X-1,NewY,-1,VelY}
    end;
obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY) when ((abs(NewY-Y)) > (abs(NewX-X))) and (VelX < 0)->
    Member = lists:any(fun({A,B}) -> NewY div 5 == B andalso X div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,NewX,Y,VelX,VelY);
        false->
            {X+1,NewY,1,VelY}
    end;
obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY) when ((abs(NewY-Y)) > (abs(NewX-X))) and (VelX== 0)->
    Member = lists:any(fun({A,B}) -> NewY div 5 == B andalso X div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,NewX,Y,VelX,VelY);
        false->
            {X,NewY,0,VelY}
    end.

% function to check if obstructed
obstructed([],_X,_Y,NewX,NewY,VelX,VelY) ->
    {NewX,NewY,VelX,VelY};
obstructed(Olist,X,Y,NewX,NewY,VelX,VelY) ->
    Member = lists:any(fun({A,B}) -> NewY div 5 == B andalso NewX div 5 == A end,Olist),
    case Member of
        true->
            obstructedmove(Olist,X,Y,NewX,NewY,VelX,VelY);
        false->
            {NewX,NewY,VelX,VelY}
    end.

jsonify_list([]) ->
    [];
jsonify_list(List) ->
    jsonify_list(List,[]).

jsonify_list([], List) ->
    List;
jsonify_list([{Dist, {Pid,{Type,{{HeadX,HeadY},{Head_X_Vel,Head_Y_Vel}}}}}|Ls], List) ->
    StringPid = list_to_binary(pid_to_list(Pid)),
    NewList = [[{id, StringPid},{type, Type}, {dist, Dist}, {x, HeadX}, {y, HeadY}, {x_velocity, Head_X_Vel}, {y_velocity, Head_Y_Vel}]| List],
    jsonify_list(Ls, NewList).

%%%%%% Entity List Management
build_entity_list (StartList, X, Y, Olist, SightLevel) ->
	DistanceList = lists:map(fun(
															{Pid,{Type,{{HX,HY},{HXV,HYV}}}}) ->
																	{abs(swarm_libs:pyth(X,Y,HX,HY)),
																	{Pid,{Type,{{HX,HY},
																	{HXV,HYV}}}}}
													end,StartList),

	FilteredList = lists:filter(
															fun({Dist,{_,{_,{{_,_},{_,_}}}}}) ->
																	Dist =< SightLevel
															end,DistanceList),

	SightList = lists:filter(
													fun({_,
															{_,{_,{{HX,HY},
															{_,_}}}}}) ->
																los:findline(X,Y,HX,HY,Olist)
															end,FilteredList),

	List = lists:keysort(1,SightList),
	%return
	List.
