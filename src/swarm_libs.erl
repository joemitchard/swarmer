-module(swarm_libs).
-export([round/2, pyth/4]).

round(Num,Precision) ->
	P = math:pow(10,Precision),
	round(Num * P) / P.

pyth(X,Y,X2,Y2)->
	math:sqrt(math:pow((X-X2),2)+math:pow((Y-Y2),2)).


