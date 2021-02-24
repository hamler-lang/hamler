-module('GlobalGroup').

-include("../../Foreign.hrl").

-export([
    globalGroups/0,
    info/0,
    monitorNodes/1,
    ownNodes/0,
    registeredNames/1,
    whereisName/1,
    whereis/2,
    send/2,
    sendInGlobal/3]).

globalGroups() ->
    ?IO(case global_group:global_groups() of
        undefined -> {'Nothing'};
        G -> {'Just', G} end).

sync_state(no_conf) -> {'NoConf'};
sync_state(synced) -> {'Synced'}.

group_tuple({Name, Nodes}) -> 
    {Name, {'PublishNormal'}, Nodes};
group_tuple({Name, PublishType, Nodes}) ->
    {Name, {case PublishType of 
                normal -> 'PublishNormal';
                hidden -> 'PublishHidden' end}, Nodes}.

info_item(Item) -> case Item of
    {state, State} -> {'State', sync_state(State)};
    {own_group_name, Nodes} -> {'OwnGroupName', Nodes};
    {own_group_nodes, Nodes} -> {'OwnGroupNodes', Nodes};
    {synced_nodes, Nodes} -> {'SyncedNodes', Nodes};
    {sync_error, Nodes} -> {'SyncError', Nodes};
    {no_contact, Nodes} -> {'NoContact', Nodes};
    {other_groups, Groups} -> {'OtherGroups', list:map(fun group_tuple/1, Groups)};
    {monitoring, Pids} -> {'Monitoring', Pids} end.

info() ->
    ?IO(list:map(fun info_item/1, global_group:info())).

monitorNodes(Flag) -> 
    global_group:monitor_nodes(Flag), 
    ?IO({ok}).

ownNodes() ->
    ?IO(global_group:own_nodes()).

location({'NodeLocation', Node}) -> {node, Node};
location({'GroupLocation', GroupName}) -> {group, GroupName}.

registeredNames(Location) ->
    ?IO(global_group:registered_names(location(Location))).


whereisName(Name) ->
    ?IO(case global_group:whereis_name(Name) of
        undefined -> {'Nothing'};
        Pid -> {'Just', Pid} end).

whereis(Location, Name) ->
    ?IO(case global_group:whereis_name(location(Location), Name) of
        undefined -> {'Nothing'};
        Pid -> {'Just', Pid} end).

send(Name, Msg) -> 
    ?IO(case global_group:send(Name, Msg) of
        {badarg, _} -> {'Nothing'};
        Pid -> {'Just', Pid} end).

sendInGlobal(Location, Name, Msg) ->
    ?IO(case global_group:send(location(Location), Name, Msg) of
        {badarg, _} -> {'Nothing'};
        Pid -> {'Just', Pid} end).
