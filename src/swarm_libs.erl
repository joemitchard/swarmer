-module(swarm_libs).
-export([round/2, pyth/4, cleanup/6, restart_proc/2, jsonify_list/1,
					obstructedmove/7]).

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
