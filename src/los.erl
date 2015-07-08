-module(los).
-export([findline/5]).

%% return a list of points, between two coordinates in intervals of +- 5

%sanity check - the two points are less than 5 away from each other
% there is no space for an obstruction - you could still check your destination
findline(X1,Y1,X2,Y2,_Olist) when (abs( X2-X1 ) < 5) and (abs( Y2-Y1 )) < 5 ->
  % in the same 5x5 cell
  error_logger:error_report("same cell"),
  true;

% Horizontal line to the right, rise = 0
findline(X1,Y1,X2,Y2,Olist) when ((Y2 div 5)-(Y1 div 5)==0) and (X1 < X2) ->
  filterlist([{CheckX,CheckY} || CheckX <- lists:seq(X1 div 5,X2 div 5,1), CheckY <- [Y1 div 5]],Olist);
% Horizontal line to the left, rise 0
findline(X1,Y1,X2,Y2,Olist) when ((Y2 div 5)-(Y1 div 5)==0) and (X1 > X2) ->
  filterlist([{CheckX,CheckY} || CheckX <- lists:seq(X1 div 5,X2 div 5,-1), CheckY <- [Y1 div 5]],Olist);

% Vertical Line down, run = 0
findline(X1,Y1,X2,Y2,Olist) when ( (X2 div 5)-(X1 div 5) == 0 ) and (Y1 < Y2) ->
  filterlist([{CheckX,CheckY} || CheckX <- [X1 div 5], CheckY <- lists:seq(Y1 div 5, Y2 div 5)],Olist);

% vertical line up, run =0
findline(X1,Y1,X2,Y2,Olist)  when ( (X2 div 5) - (X1 div 5) ==0 ) and (Y1 > Y2) ->
  filterlist([{CheckX,CheckY} || CheckX <- [X1 div 5], CheckY <- lists:seq(Y1 div 5,Y2 div 5,-1)],Olist);

% slope - find Y=MX+C
findline(X1,Y1,X2,Y2,Olist) ->
  XX = (X2 div 5)-(X1 div 5),
  YY = (Y2 div 5)-(Y1 div 5),
  case (XX == 0) and (YY == 0) of
    % this should be caught at the very top
    true ->
        true;
    false ->
      M = YY / XX,
          % Y = MX+C
          % Y-C = MX
          % -C = MX-Y
      C = ((Y1 div 5) - (M * (X1 div 5) ) ),
      findline(X1 div 5,X2 div 5,Y1 div 5,Y2 div 5,M,C,Olist)
  end.

findline(X1,X2,Y1,Y2,M,C,Olist) when X1 < X2 ->
  Xs = [{CheckX,CheckY} || CheckX <- lists:seq(X1,X2), CheckY <- [M*CheckX+C]],
  case Y1<Y2 of
      true ->
        Ys = [{CheckX,CheckY} || CheckY <- lists:seq(Y1,Y2), CheckX <- [(CheckY-C) / M]];
      false ->
        Ys=[{CheckX,CheckY} || CheckY <- lists:seq(Y1,Y2,-1), CheckX <- [(CheckY-C) / M]]
  end,
  F=sets:to_list(sets:from_list(Xs++Ys)),

  filterlist(F,Olist);

findline(X1,X2,Y1,Y2,M,C,Olist) when X1 > X2 ->
  case Y1<Y2 of
      true ->
        Xs= [{CheckXa,CheckYa} || CheckXa <- lists:seq(X1,X2,-1), CheckYa <- [M*CheckXa+C]],
        Ys = [{CheckXb,CheckYb} || CheckYb <- lists:seq(Y1,Y2), CheckXb <- [(CheckYb-C) / M]];
      false ->
        Xs= [{CheckXc,CheckYc} || CheckXc <- lists:seq(X1-1,X2,-1), CheckYc <- [M*CheckXc+C]],
        Ys= [{CheckXd,CheckYd} || CheckYd <- lists:seq(Y1,Y2,-1), CheckXd <- [(CheckYd-C) / M]]
  end,
  F=sets:to_list(sets:from_list(Xs++Ys)),
  filterlist(F,Olist).
%every element of checklist is the atom false

filterlist([],_Olist) ->
  true;
filterlist(CheckList,Olist) ->
  [H|T] = CheckList,
  {X,Y} = H,
  Reasons = {trunc(X),trunc(Y)},
  case lists:member(Reasons,Olist) of
    true ->
        false;
    false ->
        filterlist(T,Olist)
  end.
