-module(boids_functions).
-author("Robert Hales rsjh3@kent.ac.uk").

-export([super_attractor/5,super_repulsor/5,collision_avoidance/5,
	flocking/4,flocking/7,velocity/4,velocity/7,limit_speed/5,limit_speed/4]).

%Changes the velocity of the entity to move towards a super attractor.
super_attractor(X,Y,Attx,Atty,Attprcnt) ->
	Changex = (Attprcnt*(Attx-X)),
	Changey =(Attprcnt*(Atty-Y)),
	{swarm_libs:round(Changex,2),swarm_libs:round(Changey,2)}.

%Changes the velocity of the entity to move away from a super repulsor.
super_repulsor(X,Y,Repx,Repy,_Repulseprcnt) when X =:= Repx ; Y =:= Repy ->
	{0,0};
super_repulsor(X,Y,Repx,Repy,Repulseprcnt) ->
	Changex = - 1/(Repulseprcnt*(Repx-X)),
	Changey = - 1/(Repulseprcnt*(Repy-Y)),
	{swarm_libs:round(Changex,2),swarm_libs:round(Changey,2)}.

%Prevents entities from colliding.
collision_avoidance(X,Y,Otherx,Othery,Cohesionprcnt) ->
	Changex = (X - Otherx)* Cohesionprcnt,
	Changey = (Y - Othery)* Cohesionprcnt,
	{swarm_libs:round(Changex,2),swarm_libs:round(Changey,2)}.

%Makes an entity move towards the average locations of the entities in its flock.
flocking([],X,Y,_Flockprcnt) ->
	{swarm_libs:round(X,2),swarm_libs:round(Y,2)};
flocking(Next,X,Y,Flockprcnt) ->
	flocking(Next,0,0,0,X,Y,Flockprcnt).
flocking([],Count,Totalx,Totaly,X,Y,Flockprcnt)->
	Avgx = Totalx/Count,
	Avgy = Totaly/Count,
	Changex = ((Avgx - X)* Flockprcnt),
	Changey = ((Avgy - Y)* Flockprcnt),
	{swarm_libs:round(Changex,2),swarm_libs:round(Changey,2)};
flocking([H|Ts],Count,Totalx,Totaly,X,Y,Flockprcnt)->
	{_, {_,{_,{{EX,EY},{_,_}}}}} = H,
	Newcount = Count + 1,
	Newtotalx = Totalx + EX,
	Newtotaly = Totaly + EY,
	flocking(Ts,Newcount,Newtotalx,Newtotaly,X,Y,Flockprcnt).

%Makes an entity match velocity with other entities in it's flock.
velocity([],X,Y,_Velprcnt) ->
	{swarm_libs:round(X,2),swarm_libs:round(Y,2)};
velocity(Next,X,Y,Velprcnt) ->
	velocity(Next,0,0,0,X,Y,Velprcnt).
velocity([],Count,Totalx,Totaly,X,Y,Velprcnt)->
	Avgx = Totalx/Count,
	Avgy = Totaly/Count,
	Changex = ((Avgx - X)* Velprcnt),
	Changey = ((Avgy - Y)* Velprcnt),
	{swarm_libs:round(Changex,2),swarm_libs:round(Changey,2)};
velocity([H|Ts],Count,Totalx,Totaly,X,Y,Velprcnt)->
	{_, {_,{_,{{_,_},{EX,EY}}}}} = H,
	Newcount = Count + 1,
	Newtotalx = Totalx + EX,
	Newtotaly = Totaly + EY,
	velocity(Ts,Newcount,Newtotalx,Newtotaly,X,Y,Velprcnt).

%Limits the speed of an entity.
limit_speed(Limit,X,Y,Velx,Vely)->
	limit_speed(Limit,swarm_libs:pyth(X,Y,(X+Velx),(Y+Vely)),Velx,Vely).
limit_speed(Limit,Speed,Velx,Vely) when Limit < Speed ->
	Limiter = Limit/Speed,
	{swarm_libs:round(Limiter*Velx,2),swarm_libs:round(Limiter*Vely,2)};
limit_speed(_Limit,_Speed,Velx,Vely) ->
	{swarm_libs:round(Velx,2),swarm_libs:round(Vely,2)}.
