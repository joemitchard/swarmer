-module(astar).
-type cnode() :: {integer(), integer()}.
 
-define(MINX, 0).
-define(MINY, 0).
-define(MAXX, 50).%not wise
-define(MAXY, 50).
-export([
astar/3,
neighbour_nodes/3,
dist_between/2
]).

%% This module is very closely based on Srijan Choudhary's a* module [https://www.srijn.net/programming/astar-erlang-basic.html] acessed 10/3/2015
%% I've modified it to consider obstructions and used maps to replace the depreciated dicts but other than that I've changed it very little.

%% @doc Performs A* for finding a path from `Start' node to `Goal' node
-spec astar(cnode(), cnode(), list(cnode())) -> list(cnode()) | failure.
astar(Start, Goal,Obs_list) ->
  ClosedSet = sets:new(),
  OpenSet = sets:add_element(Start, sets:new()),
 
  Fscore = maps:put(Start, h_score(Start, Goal), maps:new()),
  Gscore = maps:put(Start, 0, maps:new()),
 
  CameFrom = maps:put(Start, none, maps:new()),
 
  astar_step(Goal, ClosedSet, OpenSet, Fscore, Gscore, CameFrom,Obs_list).
 
%% @doc Performs a step of A*.
%% Takes the best element from `OpenSet', evaluates neighbours, updates scores, etc..
-spec astar_step(cnode(), sets:set(), sets:set(), map(), map(), map(),list(cnode())) -> list(cnode()) | failure.
  astar_step(Goal, ClosedSet, OpenSet, Fscore, Gscore, CameFrom,Obs_list) ->
  case sets:size(OpenSet) of
    0 ->
      failure;
    _ ->
      BestStep = best_step(sets:to_list(OpenSet), Fscore, none, infinity),
    if
      Goal == BestStep ->
        lists:reverse(reconstruct_path(CameFrom, BestStep));
      true ->
        Parent = maps:get(BestStep, CameFrom),
        NextOpen = sets:del_element(BestStep, OpenSet),
        NextClosed = sets:add_element(BestStep, ClosedSet),
        Neighbours = neighbour_nodes(BestStep, Parent,Obs_list),
 
      {NewOpen, NewF, NewG, NewFrom} = scan(Goal, BestStep, Neighbours, NextOpen, NextClosed, Fscore, Gscore, CameFrom),
      astar_step(Goal, NextClosed, NewOpen, NewF, NewG, NewFrom,Obs_list)
    end
  end.
 
%% @doc Returns the heuristic score from `Current' node to `Goal' node
-spec h_score(Current :: cnode(), Goal :: cnode()) -> Hscore :: number().
h_score(Current, Goal) ->
  dist_between(Current, Goal).
 
%% @doc Returns the distance from `Current' node to `Goal' node
-spec dist_between(cnode(), cnode()) -> Distance :: number().
dist_between(Current, Goal) ->
    {X1, Y1} = Current,
    {X2, Y2} = Goal,
    abs((X2-X1)) + abs((Y2-Y1)).
 
%% @doc Returns the best next step from `OpenSetAsList'
%% TODO: May be optimized by making OpenSet an ordered set.
-spec best_step(OpenSetAsList :: list(cnode()), Fscore :: map(), BestNodeTillNow :: cnode() | none, BestCostTillNow :: number() | infinity) -> cnode().
  best_step([H|Open], Score, none, infinity) ->
  V = maps:get(H, Score),
  best_step(Open, Score, H, V);
 
  best_step([], _Score, Best, _BestValue) ->
  Best;
 
best_step([H|Open], Score, Best, BestValue) ->
  Value = maps:get(H, Score),
  case Value < BestValue of
  true ->
  best_step(Open, Score, H, Value);
  false ->
  best_step(Open, Score, Best, BestValue)
  end.
 
%% @doc Returns the neighbour nodes of `Node', and excluding its `Parent'.
-spec neighbour_nodes(cnode(), cnode(), list(cnode()) | none) -> list(cnode()).
neighbour_nodes(Node, Parent,Obs_list) ->
  {X, Y} = Node,
  Nlist=[
  {XX, YY} ||
  {XX, YY} <- [{X-1, Y}, {X, Y-1}, {X+1, Y}, {X, Y+1}],
  {XX, YY} =/= Parent,
  XX >= ?MINX,
  YY >= ?MINY,
  XX =< ?MAXX,
  YY =< ?MAXY
  ],
% added to remove obstructions from neighbours
lists:filter(fun({NX,NY}) -> not lists:any(fun({OX,OY}) -> NX==OX andalso NY==OY end,Obs_list) end,Nlist).
 
%% @doc Scans the `Neighbours' of `BestStep', and adds/updates the Scores and CameFrom mapss accordingly.
-spec scan(
Goal :: cnode(),
BestStep :: cnode(),
Neighbours :: list(cnode()),
NextOpen :: sets:set(),
NextClosed :: sets:set(),
Fscore :: map(),
Gscore :: map(),
CameFrom :: map()
) ->
{NewOpen :: sets:set(), NewF :: map(), NewG :: map(), NewFrom :: map()}.

scan(_Goal, _X, [], Open, _Closed, F, G, From) ->
{Open, F, G, From};

scan(Goal, X, [Y|N], Open, Closed, F, G, From) ->
  case sets:is_element(Y, Closed) of
    true ->
      scan(Goal, X, N, Open, Closed, F, G, From);
    false ->
      G0 = maps:get(X, G),
    TrialG = G0 + dist_between(X, Y),
      case sets:is_element(Y, Open) of
    true ->
      OldG = maps:get(Y, G),
      case TrialG < OldG of
        true ->
          NewFrom = maps:put(Y, X, From),
          NewG = maps:put(Y, TrialG, G),
          NewF = maps:put(Y, TrialG + h_score(Y, Goal), F), % Estimated total distance from start to goal through y.
          scan(Goal, X, N, Open, Closed, NewF, NewG, NewFrom);
      false ->
        scan(Goal, X, N, Open, Closed, F, G, From)
      end;
    false ->
      NewOpen = sets:add_element(Y, Open),
      NewFrom = maps:put(Y, X, From),
      NewG = maps:put(Y, TrialG, G),
      NewF = maps:put(Y, TrialG + h_score(Y, Goal), F), % Estimated total distance from start to goal through y.
      scan(Goal, X, N, NewOpen, Closed, NewF, NewG, NewFrom)
    end
  end.
 
%% @doc Reconstructs the calculated path using the `CameFrom' maps
-spec reconstruct_path(map(), cnode()) -> list(cnode()).
reconstruct_path(CameFrom, Node) ->
  case maps:get(Node, CameFrom) of
    none ->
      [Node];
    Value ->
      [Node | reconstruct_path(CameFrom, Value)]
    end.
