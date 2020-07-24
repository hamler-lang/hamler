%%---------------------------------------------------------------------------
%% |
%% Module      :  Dir
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Directory FFI module.
%%
%%---------------------------------------------------------------------------
-module('Dir').

-include("../Foreign.hrl").

-export([ makeDir/1
        , makeDirs/1
        , listDir/1
        , deleteDir/1
        , deleteDirRecursively/1
        , copyFile/2
        , deleteFile/1
        , renameFile/2
        , makeLink/2
        , makeSymLink/2
        , getLastModified/1
        , setCwd/1
        , getCwd/0
        ]).

makeDir(FilePath) ->
  ?IO(return(file:make_dir(FilePath))).

makeDirs(FilePath) ->
  ?IO(return(filelib:ensure_dir(FilePath))).

listDir(Dir) ->
  ?IO(return(file:list_dir(Dir))).

deleteDir(Dir) ->
  ?IO(return(file:del_dir(Dir))).

deleteDirRecursively(Dir) ->
  ?IO(return(file:del_dir_r(Dir))).

copyFile(Src, Dest) ->
  ?IO(case file:copy(Src, Dest) of
        {ok, _Bytes} -> ok;
        {error, Reason} -> error(Reason)
      end).

deleteFile(Filename) ->
  ?IO(return(file:delete(Filename))).

renameFile(Src, Dest) ->
  ?IO(return(file:rename(Src, Dest))).

getCwd() ->
    ?IO(return(file:get_cwd())).

%%---------------------------------------------------------------------------
%% | Symbolic links
%%---------------------------------------------------------------------------

makeLink(Source, Target) ->
  ?IO(return(file:make_link(Source, Target))).

makeSymLink(Source, Target) ->
  ?IO(return(file:make_symlink(Source, Target))).

%%---------------------------------------------------------------------------
%% | File timestamps
%%---------------------------------------------------------------------------

getLastModified(Path) ->
  ?IO(filelib:last_modified(Path)).

%%---------------------------------------------------------------------------
%% | Working directory
%%---------------------------------------------------------------------------

setCwd(Dir) ->
  ?IO(return(file:set_cwd(Dir))).

%%---------------------------------------------------------------------------
%% | Internal functions
%%---------------------------------------------------------------------------

return(ok) -> ok;
return({ok, Result}) -> Result;
return({error, Reason}) -> error(Reason).
