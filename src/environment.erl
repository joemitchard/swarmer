-module(environment).
-author("Jake Pearse jp480@kent.ac.uk").

-behaviour(gen_server).

-include_lib("include/swarmer.hrl").

%%% API
-export([start_link/0,make_grid/4,get_grid_info/0,report/0,
         pause_entities/0, unpause_entities/0, start_entities/0,
         type_pause_unpause/2,create_obs/2]).

%%%% internal functions for debugging these can be deleted later
-export([get_state/0,set_swarm/1,set_mob/1,set_items/1]).

%%%% gen_server callbacks
-export([code_change/3,handle_call/2,handle_call/3,handle_cast/2,
  handle_info/2,init/1,terminate/2]).

-define(SERVER, ?MODULE).
-define(ZOMBIE_TIMEOUT, 600).
-define(HUMAN_TIMEOUT, 600).

% Record to Props List, for reporting
-define(R2P(Record), record_to_propslist(#state{} = Record) ->
            lists:zip(record_info(fields, Record), tl(tuple_to_list(Record)))).

-record(state,{
% map viewer PID's to respective tiles
  viewerPropList,
% Number of tile rows
  rows,
% number of tile columns
  columns,
% size of each tile
  tileSize,
% list of obstructions
  obs_list = []
  }).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get general information about the grid
%%%% @end
%%%%------------------------------------------------------------------------------
get_grid_info() ->
    gen_server:call(?MODULE,grid_info).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Get the entire #state for debugging
%%%% @end
%%%%------------------------------------------------------------------------------
get_state() ->
    gen_server:call(?MODULE,get_state).

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Make a new tile grid, stored in #state
%%%% @end
%%%%------------------------------------------------------------------------------
make_grid(Rows,Columns,TileSize,Obs_list) ->
  gen_server:call(?MODULE,{make_grid,{Rows,Columns,TileSize,Obs_list}}).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% retrive the state of the whole environment
%%%% @end
%%%%------------------------------------------------------------------------------
report() ->
    gen_server:call(?MODULE,report).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% create Num zombies and add add them to the tiles
%%%% @end
%%%%------------------------------------------------------------------------------
set_swarm(Num) ->
  gen_server:call(?MODULE,{swarm,Num}).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% create Num zombies and add add them to the tiles
%%%% @end
%%%%------------------------------------------------------------------------------
set_mob(Num) ->
  gen_server:call(?MODULE,{mob,Num}).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% Place items on the map
%%%% @end
%%%%------------------------------------------------------------------------------
set_items(Num) ->
  gen_server:call(?MODULE,{items,Num}).

%%%%------------------------------------------------------------------------------
%%%% @doc
%% create Num zombies and add add them to the tiles
%%%% @end
%%%%------------------------------------------------------------------------------
start_entities() ->
  do_start_entities().

%%%%------------------------------------------------------------------------------
%%%% @doc
%% Pauses all running entities
%%%% @end
%%%%------------------------------------------------------------------------------
pause_entities() ->
  do_pause_entities().

%%%%------------------------------------------------------------------------------
%%%% @doc
%% Unpauses all running entities
%%%% @end
%%%%------------------------------------------------------------------------------
unpause_entities() ->
  do_unpause_entities().

%%%%------------------------------------------------------------------------------
%%%% @doc
%% Pauses or unpauses entities of a specific type.
%%%% @end
%%%%------------------------------------------------------------------------------
type_pause_unpause(Action,Type) ->
  do_action_entities_type(Action,Type).

create_obs(Obs_list,TileSize) ->
	gen_server:call(?MODULE,{create_obs_map,Obs_list,TileSize}).


%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([]) ->
   {ok, #state{}}. %new state record with default values

%%% calls
handle_call(report,_From,State) ->
    Report = make_report(),
    % error_logger:error_report(Report),
    {reply,Report,State};


handle_call(grid_info,_From,State) ->
  Rows = State#state.rows,
  Columns = State#state.columns,
  Size = State#state.tileSize,
  Obs = lists:map(fun({T,{X,Y}}) -> [T,X,Y] end,State#state.obs_list),
  {reply,[{<<"rows">>,Rows},{<<"columns">>,Columns},{<<"tileSize">>,Size},{<<"obs">>,Obs}],State};

handle_call(get_state,_From,State) ->
    {reply,State,State};

handle_call({make_grid,{Rows,Columns,TileSize,Obs_list}},_From,State) ->
  % End current environment setup, respawn after killing them
  % This prevents old processes being left around
  %kill entities
  supervisor:terminate_child(swarm_sup, zombie_sup),
  supervisor:terminate_child(swarm_sup, human_sup),
  supervisor:terminate_child(swarm_sup, supplies_sup),
  supervisor:restart_child(swarm_sup, zombie_sup),
  supervisor:restart_child(swarm_sup, human_sup),
  supervisor:restart_child(swarm_sup, supplies_sup),
  %Kill tiles
  supervisor:terminate_child(swarm_sup, tile_sup),
  supervisor:restart_child(swarm_sup, tile_sup),
  %Kill viewers
  supervisor:terminate_child(swarm_sup, viewer_sup),
  supervisor:restart_child(swarm_sup, viewer_sup),
  Grid = populate_grid(Rows,Columns,TileSize),

  Viewers=add_viewers(Grid),
  % error_logger:error_report(Viewers),

  make_neighbourhood(Grid,Viewers),
  %error_logger:error_report(State#state.viewerPropList),

  Cord_list = lists:map(fun(I)-> X=I rem (10*Rows), Y=I div (10*Columns), T = list_to_atom("tile" ++  "X" ++ integer_to_list(X div 10) ++  "Y" ++ integer_to_list(Y div 10)),{T,{X,Y}} end,Obs_list),
    %error_logger:error_report(Cord_list),
	lists:foreach(fun(K) -> tile:set_obs_list(K,proplists:get_all_values(K,Cord_list)) end,proplists:get_keys(Cord_list)),


  {reply,ok,State#state{rows=Rows,columns=Columns,tileSize=TileSize,viewerPropList=Viewers,obs_list=Cord_list}};

handle_call({swarm,Num},_From,State) ->
  %kill entities
  supervisor:terminate_child(swarm_sup, zombie_sup),
  supervisor:restart_child(swarm_sup, zombie_sup),
  create_swarm(State,Num),
  do_action_entities_type(pause, zombies),
  {reply,ok,State};

handle_call({mob,Num},_From,State) ->
  %kill entities
  supervisor:terminate_child(swarm_sup, human_sup),
  supervisor:restart_child(swarm_sup, human_sup),
  create_mob(State,Num),
  do_action_entities_type(pause, humans),
  {reply,ok,State};

handle_call({items,Num},_From,State) ->
  %kill entities
  supervisor:terminate_child(swarm_sup, supplies_sup),
  supervisor:restart_child(swarm_sup, supplies_sup),
  place_items(State,Num),
  {reply,ok,State}.

%%% map a list of integers to [{Tile,{X,Y}},...] and pushes them into the relevant tile's State#state.obs_list
%handle_call({create_obs_map,Obs_list,GridSize},_From,State) ->
	%Cord_list = lists:map(fun(I)-> X=I rem 50, Y=I div 50, T = list_to_atom("tile" ++  "X" ++ integer_to_list(X div 10) ++  "Y" ++ integer_to_list(Y div 10)),{T,{X,Y}} end,Obs_list),
    %error_logger:error_report(Cord_list),
	%lists:foreach(fun(K) -> tile:set_obs_list(K,proplists:get_all_values(K,Cord_list)) end,proplists:get_keys(Cord_list)),

  %{reply,ok,State#state{obs_list = Cord_list}}.

handle_call(terminate,State) ->
  {stop,normal,State}.

% other gen_server stuff

%%%% Handle cast to end the system normally
handle_cast(terminate, State) ->
    {stop,normal,State}.

handle_info(Msg,State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
    {noreply,State}.

terminate(normal,_State) ->
    ok.

code_change(_OldVsn, State,_Extra) ->
    {ok,State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================


%%%% called by populate grid to make a row of tiles %%%%%%%
make_row(RowCounter,Columns,ColumnCounter,TileSize) ->
  make_row(RowCounter,Columns,ColumnCounter,TileSize,[]).
make_row(_RowCounter,Columns,ColumnCounter,_TileSize,Row) when ColumnCounter > Columns -1 ->
  Row;
make_row(RowCounter,Columns,ColumnCounter,TileSize,Row) ->
  Name = list_to_atom("tile" ++  "X" ++ integer_to_list(ColumnCounter) ++  "Y" ++ integer_to_list(RowCounter)),
  {ok,_} = supervisor:start_child(tile_sup,
                                        [Name,
                                        ColumnCounter*TileSize,
                                        RowCounter*TileSize,
                                        TileSize]),
  make_row(RowCounter,Columns,ColumnCounter +1,TileSize, Row++[Name]).

%%% populate could be misleading, it means populate a grid with tiles %%%
populate_grid(Rows,Columns,TileSize) ->
  populate_grid(0,Rows,Columns,TileSize,[]).
populate_grid(RowCounter,Rows,_Columns,_TileSize,Grid) when RowCounter > Rows -1 ->
  Grid;
populate_grid(RowCounter,Rows,Columns,TileSize,Grid) ->
  populate_grid(RowCounter +1,Rows,Columns,TileSize,Grid++make_row(RowCounter,Columns,0,TileSize)).

%%% take a list of tiles and add viewers to each one
add_viewers(Grid) ->
 add_viewers(Grid,[]).
add_viewers([],Viewers) ->
  Viewers;
add_viewers(Grid,Viewers) ->
  [H|T]=Grid,
  {ok,V}=supervisor:start_child(viewer_sup,[]),
  tile:set_viewer(H,V),
  add_viewers(T,Viewers ++ [{H,V}]).


%% Spawns Num randomly positioned zombies
create_swarm(#state{tileSize = TileSize, columns = Columns, rows = Rows, obs_list = Obs_List} = State,Num) ->
    lists:foreach(
        fun(_) ->
            {Xpos,Ypos} = avoidObs(Obs_List,TileSize,Rows),
            {Tile,Viewer} = get_tile(Xpos,Ypos,State),
            {ok,Zombie}=supervisor:start_child(zombie_sup,[Xpos,Ypos,Tile,TileSize,Columns,Rows,Viewer,?ZOMBIE_TIMEOUT,0]),
            zombie_fsm:start(Zombie)
        end,lists:seq(1,Num)).

%% Spawns Num randomly positioned humans
create_mob(#state{tileSize = TileSize, columns = Columns, rows = Rows, obs_list = Obs_List} = State,Num) ->
    lists:foreach(
        fun(_) ->
            {Xpos,Ypos} = avoidObs(Obs_List,TileSize,Rows),
            {Tile,Viewer} = get_tile(Xpos,Ypos,State),
            {ok,Human}=supervisor:start_child(human_sup,[Xpos,Ypos,Tile,TileSize,Columns,Rows,Viewer,?HUMAN_TIMEOUT]),
            human_fsm:start(Human)
        end,lists:seq(1,Num)).

% creates and places a number of randomly positioned supplies.
place_items(#state{tileSize = TileSize, rows = Rows, obs_list = Obs_List} = State,Num) ->
    lists:foreach(
        fun(_) ->
            {Xpos,Ypos} = avoidObs(Obs_List,TileSize,Rows),
            {Tile,Viewer} = get_tile(Xpos,Ypos,State),
            % draw to pick type of item for randomisation
            case random:uniform(3) of
              1 ->
                supervisor:start_child(supplies_sup,[food, apple, Xpos,Ypos,Tile,Viewer]);
              2 ->
                supervisor:start_child(supplies_sup,[food, orange, Xpos,Ypos,Tile,Viewer]);
              3 ->
                supervisor:start_child(supplies_sup,[food, banana, Xpos,Ypos,Tile,Viewer])
            end
        end,lists:seq(1,Num)).

%% search for a tile by X,Y in the viewerPropList
get_tile(Xpos,Ypos,#state{viewerPropList = ViewerPropList, tileSize = TileSize}) ->
  Tile = list_to_atom("tile" ++  "X" ++ integer_to_list(Xpos div TileSize) ++  "Y" ++ integer_to_list(Ypos div TileSize)),
  Viewer = proplists:get_value(Tile,ViewerPropList),
  {Tile,Viewer}.

%% Makes a report for the client.
%% This report contains a props list of all the states for each supervisors children.
make_report() ->
    % Build a list of entities from the supervisors
    Entities = lists:filtermap(
        fun({_Id, Pid, _Type, [Module]}) ->
            case Module:get_state(Pid) of
                {ok,StateData} ->
                    {true, StateData};
                _ ->
                    false
            end
        end, get_entities_list()),

    % Build a list of items from the supervisor
    Items = lists:filtermap(
        fun({_Id, Pid, _Type, [Module]}) ->
            case Module:get_state(Pid) of
                {ok,StateData} ->
                    {true, StateData};
                _ ->
                    false
            end
        end, get_supplies_list()),

    % Remove all items that have been eaten
    NonTakenItems = lists:filter(
        fun([_,{_,Bool},_,_,_]) ->
            Bool == false
        end,Items),

    Entities ++ NonTakenItems.


make_neighbourhood(TileList,ViewerPropList) ->
  ViewerGeomList = setup_neighbours(ViewerPropList),
  do_make_neighbourhood(TileList,ViewerGeomList).
do_make_neighbourhood([],_) -> ok;
do_make_neighbourhood(TileList,ViewerGeomList) ->
  [T|Ts] = TileList,
  {Xo,Yo,_,_,_Size} = tile:get_geometry(T),
  tile:set_neighbours(T,get_neighbours(Xo,Yo,ViewerGeomList)),
  do_make_neighbourhood(Ts,ViewerGeomList).

setup_neighbours(ViewerPropList) ->
  %reformat the viewer propslist inot one with geometry
  lists:map(fun({T,V})-> {tile:get_geometry(T),V} end, ViewerPropList).


get_neighbours(Xo,Yo,ViewersWithGeometry) ->
  get_neighbours(Xo,Yo,ViewersWithGeometry,[]).

get_neighbours(_,_,[],NeighbourList) -> NeighbourList;
get_neighbours(Xo,Yo,ViewersWithGeometry,NeighbourList) ->
  [V|Vs]=ViewersWithGeometry,
  {{X,Y,_,_,Size},Viewer} = V,
  case test_neighbour (Xo,Yo,X,Y,Size) of
    true ->
      NewList =NeighbourList ++ [Viewer],
      get_neighbours(Xo,Yo,Vs,NewList);
    false -> get_neighbours(Xo,Yo,Vs,NeighbourList)
  end.


test_neighbour(Xo,Yo,X,Y,Size) ->
      (X =:= Xo + Size) or (X =:= Xo) or (X =:= Xo - Size)
      andalso
      (Y =:= Yo + Size) or (Y =:= Yo) or (Y =:= Yo - Size).

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

avoidObs(Ob_List,TileSize,Rows) ->
  random:seed(now()),
  X =random:uniform(TileSize*Rows-1),
  Y =random:uniform(TileSize*Rows-1),
  case lists:any(fun({_T,{A,B}}) -> {X div 5,Y div 5}=={A,B} end,Ob_List) of
    true ->
      avoidObs(Ob_List,TileSize,Rows);
    false ->
      {X,Y}
  end.
