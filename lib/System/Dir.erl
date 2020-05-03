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
%% The Directory FFI.
%%
%%---------------------------------------------------------------------------
-module('Dir').

-export([ %% Actions on directories
          createDirectory/1
        , createDirectoryIfMissing/2
        , removeDirectory/1
        , removeDirectoryRecursive/1
        , removePathForcably/1
nameDirectory :: FilePath -> FilePath -> IO Unit
listDirectory :: FilePath -> List FilePath
getCurrentDirectory :: IO FilePath
setCurrentDirectory :: FilePath -> IO Unit

-- Pre-defined directories
getHomeDirectory :: IO FilePath
getUserDocumentsDirectory :: IO FilePath
getTemporaryDirectory :: IO FilePath

-- Actions on files
removeFile :: FilePath -> IO Unit
nameFile :: FilePath -> FilePath -> IO Unit
namePath :: FilePath -> FilePath -> IO Unit
copyFile :: FilePath -> FilePath -> IO Unit
getFileSize :: FilePath -> IO Integer

makeAbsolute :: FilePath -> IO FilePath


-- Existence tests
doesPathExist :: FilePath -> IO Boolean
doesFileExist :: FilePath -> IO Boolean
doesDirectoryExist :: FilePath -> IO Boolean

-- Symbolic links
createFileLink :: FilePath -> FilePath -> IO Unit
createDirectoryLink :: FilePath -> FilePath -> IO Unit
removeDirectoryLink :: FilePath -> IO Unit
pathIsSymbolicLink :: FilePath -> IO Boolean
getSymbolicLinkTarget :: FilePath -> IO FilePath

-- Permissions
getPermissions :: FilePath -> IO Permissions
setPermissions :: FilePath -> Permissions -> IO Unit
copyPermissions :: FilePath -> FilePath -> IO Unit

-- timestamps
getAccessTime :: FilePath -> IO UTCTime
getModificationTime :: FilePath -> IO UTCTime
setAccessTime :: FilePath -> UTCTime -> IO Unit
setModificationTime :: FilePath -> UTCTime -> IO Unit

-- $PATH methods
getSearchPath :: IO [FilePath]

-- extension functions
splitExtension :: FilePath -> (String,String)
takeExtension :: FilePath -> String
replaceExtension :: FilePath -> String -> FilePath
addExtension :: FilePath -> String -> FilePath

-- filename/directory functions
getFileName :: FilePath -> String
combine :: FilePath -> FilePath -> FilePath




        
        
        ]).




%% FilePath -> IO Unit
createDirectory(FilePath) ->

%% Boolean -> FilePath -> IO Unit
createDirectoryIfMissing

%% FilePath -> IO Unit
removeDirectory

%% FilePath -> IO Unit
removeDirectoryRecursive

%% FilePath -> IO Unit
removePathForcably

%% FilePath -> FilePath -> IO Unit
nameDirectory

%% FilePath -> List FilePath
listDirectory

%% IO FilePath
getCurrentDirectory

%% FilePath -> IO Unit
setCurrentDirectory

%% Pre-defined directories

%% IO FilePath
getHomeDirectory

%% IO FilePath
getUserDocumentsDirectory

%% IO FilePath
getTemporaryDirectory

%% Actions on files

%% FilePath -> IO Unit
removeFile

%% FilePath -> FilePath -> IO Unit
nameFile

%% FilePath -> FilePath -> IO Unit
namePath

%% FilePath -> FilePath -> IO Unit
copyFile

%% FilePath -> IO Integer
getFileSize

%% FilePath -> IO FilePath
makeAbsolute

%% Existence tests

%% FilePath -> IO Boolean
doesPathExist

%% FilePath -> IO Boolean
doesFileExist

%% FilePath -> IO Boolean
doesDirectoryExist

%% Symbolic links

%% FilePath -> FilePath -> IO Unit
createFileLink

%% FilePath -> FilePath -> IO Unit
createDirectoryLink

%% FilePath -> IO Unit
removeDirectoryLink

%% FilePath -> IO Boolean
pathIsSymbolicLink

%% FilePath -> IO FilePath
getSymbolicLinkTarget

%% FilePath -> IO Permissions
getPermissions

%% FilePath -> Permissions -> IO Unit
setPermissions

%% FilePath -> FilePath -> IO Unit
copyPermissions

%% timestamps

%% FilePath -> IO UTCTime
getAccessTime

%% FilePath -> IO UTCTime
getModificationTime

%% FilePath -> UTCTime -> IO Unit
setAccessTime

%% FilePath -> UTCTime -> IO Unit
setModificationTime

%% $PATH methods
getSearchPath :: IO [FilePath]

%% extension functions

%% FilePath -> (String,String)
splitExtension

%% FilePath -> String
takeExtension

%% FilePath -> String -> FilePath
replaceExtension

%% FilePath -> String -> FilePath
addExtension

%% filename/directory functions

%% FilePath -> String
getFileName

%% FilePath -> FilePath -> FilePath
combine

