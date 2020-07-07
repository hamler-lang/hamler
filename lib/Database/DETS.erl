%%---------------------------------------------------------------------------
%% |
%% Module      :  DETS
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The DETS FFI module.
%%
%%---------------------------------------------------------------------------
-module('DETS').

-export([ delete/2
        , deleteAllObjects/1
        , deleteObject/2
        , first/1
        , fromETS/2
        , info/1
        , insert/2
        , insertNew/2
        , lookup/2
        , member/2
        , next/2
        , toETS/2
        ]).

delete(Name, Key) ->
  return(dets:delete(Name, Key)).

deleteAllObjects(Name) ->
  return(dets:delete_all_objects(Name)).

deleteObject(Name, Object) ->
  return(dets:delete_object(Name, Object)).

first(Name) ->
  case dets:first(Name) of
    '$end_of_table' -> {'Nothing'};
    Key -> {'Just', Key}
  end.

fromETS(Name, EtsTab) ->
  return(dets:from_ets(Name, EtsTab)).

info(Name) ->
  case dets:info(Name) of
    undefined -> {'Nothing'};
    Info -> {'Just', infoRec(Info, #{})}
  end.

insert(Name, Objects) ->
  return(dets:insert(Name, Objects)).

insertNew(Name, Objects) ->
  return(dets:insert_new(Name, Objects)).

lookup(Name, Key) ->
  return(dets:lookup(Name, Key)).

member(Name, Key) ->
  return(dets:member(Name, Key)).

next(Name, Key1) ->
  case dets:next(Name, Key1) of
    '$end_of_table' -> {'Nothing'};
    Key2 -> {'Just', Key2}
  end.

toETS(Name, EtsTab) ->
  return(dets:to_ets(Name, EtsTab)).

return({error, Reason}) -> error(Reason);
return(Res) -> Res.

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

infoRec([{file_size, Size}|Info], M) ->
  infoRec(Info, M#{fileSize => Size});
infoRec([{filename, Name}|Info], M) ->
  infoRec(Info, M#{filename => Name});
infoRec([{keypos, Pos}|Info], M) ->
  infoRec(Info, M#{keyPos => Pos});
infoRec([{size, Size}|Info], M) ->
  infoRec(Info, M#{size => Size});
infoRec([{type, Type}|Info], M) ->
  infoRec(Info, M#{tableType => Type});
infoRec([_|Info], M) ->
  infoRec(Info, M);
infoRec([], M) -> M.

