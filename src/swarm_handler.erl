-module(swarm_handler).

%-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
%-export([handle/2]).
-export([terminate/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {}).

% take any request from tcp or http and push it to websockets
init({tcp,http},_Req,_Opts) ->
	%error_logger:error_report("init started"),
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #state{}}.
        
websocket_handle({text, Json}, Req, State) ->
    Parsed = jsx:decode(Json),
    Type = proplists:get_value(<<"type">>,Parsed),
    case Type of
      <<"setup">> ->
        Arrity = proplists:get_value(<<"arrity">>,Parsed),
        Size = proplists:get_value(<<"swarmSize">>,Parsed),
        Pop = proplists:get_value(<<"popSize">>,Parsed),
        Obs= proplists:get_value(<<"obArray">>,Parsed),
        Items = proplists:get_value(<<"items">>,Parsed),
        % Now TileSize must be a hardcoded value.
        TileSize=50,
        environment:make_grid(Arrity,Arrity,TileSize,Obs),
        GridInfo = [environment:get_grid_info()],
        % create_obs pushes all the obstructed coordinates into environment
        % it returns ok, which I dont need any more.
        environment:set_swarm(Size),
        environment:set_mob(Pop),
        environment:set_items(Items),
        Report = environment:report(),
        Status=jsx:encode(Report++GridInfo),
        {reply, [{text,Status}], Req, State};

      <<"report">> ->
        Report = jsx:encode(environment:report()),
        {reply, [{text,Report}], Req, State};
      
      <<"start">> ->
		environment:unpause_entities(),
		{reply,[{text,"ok"}], Req, State};

    <<"pause">> ->
      environment:pause_entities(),
      {reply,[{text,"ok"}], Req, State};
      
      _ ->
    error_logger:error_report({"Unknown message type", Type}),
              {noreply, Req, State}
    end;

websocket_handle(_Any, Req, State) ->
  {reply, [{text, << "what the duce?" >>}], Req, State}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

terminate(_Reason, _Req, _State) ->
  error_logger:error_report("someother term started"),
  ok.
