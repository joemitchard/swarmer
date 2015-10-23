-module(swarm_libs).
-export([round/2, pyth/4, cleanup/6]).

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
