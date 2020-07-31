%%---------------------------------------------------------------------------
%% |
%% Module      :  PG
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The PG FFI module.
%%
%%---------------------------------------------------------------------------
-module('PG').

-include("../../Foreign.hrl").

-export([ startLink/0
        , startScope/1
        , startLinkScope/1
        , join/2
        , joinScope/3
        , leave/2
        , leaveScope/3
        , getLocalMembers/1
        , getLocalMembersIn/2
        , getMembers/1
        , getMembersIn/2
        , whichGroups/0
        , whichGroupsIn/1
        ]).

startLink() ->
  ?IO(retPid(pg:start_link())).

startScope(Scope) ->
  ?IO(retPid(pg:start(Scope))).

startLinkScope(Scope) ->
  ?IO(retPid(pg:start_link(Scope))).

join(Group, Pids) ->
  ?IO(pg:join(Group, Pids)).

joinScope(Scope, Group, Pids) ->
  ?IO(pg:join(Scope, Group, Pids)).

leave(Group, Pids) ->
  ?IO(pg:leave(Group, Pids)).

leaveScope(Scope, Group, Pids) ->
  ?IO(pg:leave(Scope, Group, Pids)).

getLocalMembers(Group) ->
  ?IO(pg:get_local_members(Group)).

getLocalMembersIn(Scope, Group) ->
  ?IO(pg:get_local_members(Scope, Group)).

getMembers(Group) ->
  ?IO(pg:get_members(Group)).

getMembersIn(Scope, Group) ->
  ?IO(pg:get_members(Scope, Group)).

whichGroups() ->
  ?IO(pg:which_groups()).

whichGroupsIn(Scope) ->
  ?IO(pg:which_groups(Scope)).

retPid({ok, Pid}) -> Pid;
retPid({error, Reason}) -> error(Reason).
