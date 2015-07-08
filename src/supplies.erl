-module(supplies).
-author("Joe Mitchard jm710@kent.ac.uk").

-behaviour(gen_server).

%%%% API
-export([start_link/6, 
			get_type/1, 
			get_state/1,
			picked_up/1]).

%%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{	id,
				taken_bool,
				type,
				item,
				viewer,
				tile,
				x,
				y}).

%%%%%%=============================================================================
%%%%%% API
%%%%%%=============================================================================

%%%%------------------------------------------------------------------------------
%%%% @doc
%%%% Start the server.
%%%% @end
%%%%------------------------------------------------------------------------------
start_link(Type, Item, X, Y, Tile, Viewer) ->
    gen_server:start_link(?MODULE, [Type, Item, X, Y, Tile, Viewer], []).


get_type(Pid) ->
	gen_server:call(Pid,get_type).

get_state(Pid) ->
	gen_server:call(Pid,get_state).

picked_up(Pid) ->
	gen_server:call(Pid,picked_up).

%%%%%%=============================================================================
%%%%%% gen_server Callbacks
%%%%%%=============================================================================

init([Type, Item, X, Y, Tile, Viewer]) ->
	tile:place_item(Tile, {self(),X,Y,Type,Item}),
    {ok, #state{id = self(), taken_bool = false, type = Type, x = X, y = Y, 
    			tile = Tile, viewer = Viewer}}.

handle_call(get_type, _From, #state{type = Type, item = Item} = State) ->
	{reply, {Type, Item},State};
	
handle_call(get_state, _From, #state{taken_bool = TakenBool, x = X, y = Y} = State) ->
	{reply,{ok,[{id,list_to_binary(pid_to_list(self()))},{taken_bool,TakenBool},{type,food},{x,X},{y,Y}]},State};

handle_call(picked_up, _From, #state{tile = Tile, taken_bool = TakenBool} = State) ->
	% error_logger:error_report("I've been eaten!"),
	case TakenBool of
		false ->
			tile:remove_item(Tile, self()),
			{reply, ok, State#state{taken_bool = true}};
		true ->
			{reply, no, State}
	end;
handle_call(Request, _From, State) ->
    {stop, unexpected_call, {undefined, Request}, State}.

handle_cast(_Request, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%=============================================================================
%%%%%% Internal Functions
%%%%%%=============================================================================
