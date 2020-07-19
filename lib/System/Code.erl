%%---------------------------------------------------------------------------
%% |
%% Module      :  Code
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Code FFI module.
%%
%%---------------------------------------------------------------------------
-module('Code').

-include("../Foreign.hrl").

-export([ addPath/1
        , allAvailable/0
        , allLoaded/0
        , delPath/1
        , ensureLoaded/1
        , libDirOf/1
        , privDir/1
        ]).

addPath(Path) ->
  ?IO(case code:add_path(Path) of
        true -> ok;
        {error, Reason} -> error(Reason)
      end).

delPath(Path) ->
  ?IO(case code:del_path(Path) of
        Succ when is_boolean(Succ) -> Succ;
        {error, Reason} -> error(Reason)
      end).

allAvailable() ->
  ?IO([{Module, fixAtom(Filename), Loaded}
       || {Module, Filename, Loaded} <- code:all_available()]).

allLoaded() ->
  ?IO([{Module, fixAtom(Filename)}
       || {Module, Filename} <- code:all_loaded()]).

ensureLoaded(Module) ->
  ?IO(case code:ensure_loaded(Module) of
        {module, _} -> ok;
        {error, Reason} -> error(Reason)
      end).

libDirOf(Module) ->
  ?IO(case code:lib_dir(Module) of
        {error, bad_name} -> error(bad_name);
        FilePath -> FilePath
      end).

privDir(Module) ->
  ?IO(case code:priv_dir(Module) of
        {error, bad_name} -> error(bad_name);
        FilePath -> FilePath
      end).

fixAtom(A) when is_atom(A) ->
  atom_to_list(A);
fixAtom(S) -> S.


