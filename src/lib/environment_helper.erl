-module(environment_helper).
% module for the internal functions within Environment to make the module a
% little more managable...
-export([make_report/0]).

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
        end, swarm_libs:get_entities_list()),

    % Build a list of items from the supervisor
    Items = lists:filtermap(
        fun({_Id, Pid, _Type, [Module]}) ->
            case Module:get_state(Pid) of
                {ok,StateData} ->
                    {true, StateData};
                _ ->
                    false
            end
        end, swarm_libs:get_supplies_list()),

    % Remove all items that have been eaten
    NonTakenItems = lists:filter(
        fun([_,{_,Bool},_,_,_]) ->
            Bool == false
        end,Items),

    Entities ++ NonTakenItems.
