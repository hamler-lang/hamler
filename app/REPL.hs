{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module REPL where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT,get)
import Data.Foldable (for_)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import Minteractive
import qualified Options.Applicative as Opts
import System.Console.Haskeline
import System.Directory (doesDirectoryExist, getTemporaryDirectory, createDirectory, getCurrentDirectory, listDirectory)
import System.Exit
import System.FilePath.Glob (glob)
import System.IO (BufferMode (..), Handle, hGetLine, hSetBuffering)
import System.Process
import qualified Shelly as SS
import qualified Data.Text as T
import qualified Control.Exception as CE
import Version (hamlerEnv)
import System.FilePath.Posix ((</>))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class
import Control.Monad.Catch

hamlerlib :: String
hamlerlib = let vp = unsafePerformIO $ lookupEnv "HAMLER_HOME"
            in case vp of
                 Nothing -> $hamlerEnv </> "lib"
                 Just v  -> v </> "lib"

hamlerFile :: String
hamlerFile = let vp = unsafePerformIO $ lookupEnv "HAMLER_HOME"
             in case vp of
                  Nothing -> $hamlerEnv
                  Just v  -> v


data PSCiOptions
  = PSCiOptions
      { psciInputGlob :: [(String, Bool)],
        psciBackend :: Backend
      }

inputFile :: Opts.Parser FilePath
inputFile =
  Opts.strArgument $
    Opts.metavar "FILES"
      <> Opts.help "Optional .purs files to load on start"

data Backend
  = forall state.
    Backend
      { -- | Initialize, and call the continuation when the backend is ready
        _backendSetup :: IO state,
        -- | Evaluate JavaScript code
        _backendEval :: state -> String -> IO (),
        -- | Reload the compiled code
        _backendReload :: state -> IO (),
        -- | Shut down the backend
        _backendShutdown :: state -> IO ()
      }

nodeBackend :: Backend
nodeBackend = Backend setup eval reload shutdown
  where
    setup :: IO ()
    setup = return ()
    eval :: () -> String -> IO ()
    eval _ _ = return ()
    reload :: () -> IO ()
    reload _ = return ()
    shutdown :: () -> IO ()
    shutdown _ = return ()


-- | Parses the input and returns either a command, or an error as a 'String'.
getCommand :: forall m. (MonadMask m, MonadIO m) => String -> InputT m (Either String [Command])
getCommand s = handleInterrupt (return (Right [])) $ do
  line <- withInterrupt $ getInputLine $ addSpace s
  case line of
    Nothing -> return (Right [QuitPSCi]) -- Ctrl-D when input is empty
    Just "" -> return (Right [])
    Just s  -> return (parseCommand s)

addSpace :: String -> String
addSpace w = case words w of
              [r] -> r++" "
              _ -> w

pasteMode :: forall m. (MonadMask m, MonadIO m) => InputT m (Either String [Command])
pasteMode =
    parseCommand <$> go []
  where
    go :: [String] -> InputT m String
    go ls = maybe (return . unlines $ reverse ls) (go . (:ls)) =<< getInputLine "… "

ishmFile :: String -> Bool
ishmFile fname = (== "mh.") $ take 3 $ reverse $ fname

gethmFiles :: FilePath -> IO [FilePath]
gethmFiles basePath = do
  list <- listDirectory basePath
  r <- forM list $ \filePath -> do
    let tp = basePath ++ "/" ++ filePath
    res <- doesDirectoryExist tp
    if res
      then gethmFiles tp
      else
        if ishmFile filePath
          then return [tp]
          else return []
  return $ concat r

hmfs :: IO [(FilePath, Bool)]
hmfs = do
  dir <- getCurrentDirectory
  fmap (\x -> (x, False)) <$> gethmFiles (dir <> "/lib")

srchmfs :: IO [(FilePath, Bool)]
srchmfs = do
  dir <- getCurrentDirectory
  t1 <- gethmFiles (dir <> "/src")
  t2 <- gethmFiles hamlerlib
  return $ ( zip t1 $ repeat False) <> (zip t2 $ repeat True)


nullhmfs :: IO [(FilePath, Bool)]
nullhmfs = do
  t2 <- gethmFiles hamlerlib
  return $ (zip t2 $ repeat True)

nullhmfs' :: FilePath -> IO [(FilePath, Bool)]
nullhmfs' fp = do
  t1 <- gethmFiles fp
  t2 <- gethmFiles hamlerlib
  return $ ( zip t1 $ repeat False) <> (zip t2 $ repeat True)

dout :: Handle -> IO ()
dout h = do
  l <- hGetLine h
  putStrLn l
  dout h

data ReplConfig
  = ReplConfig
      { replsrvFilePath :: FilePath,
        hamlerFiles :: IO [(FilePath, Bool)],
        libBeamPath :: FilePath,
        srcBeamPath :: FilePath,
        coreFilePath :: FilePath
      }

devReplConfig :: ReplConfig
devReplConfig =
  ReplConfig
    { replsrvFilePath = "repl/replsrv",
      hamlerFiles = hmfs,
      libBeamPath = "ebin",
      srcBeamPath = "ebin",
      coreFilePath = "ebin/$REPL.core"
    }

srcReplConfig :: ReplConfig
srcReplConfig =
  ReplConfig
    { replsrvFilePath = "/usr/local/lib/hamler/bin/replsrv",
      hamlerFiles = srchmfs,
      libBeamPath = "/usr/local/lib/hamler/ebin",
      srcBeamPath = "ebin",
      coreFilePath = "ebin/$REPL.core"
    }

nullReplConfig :: ReplConfig
nullReplConfig =
  ReplConfig
    { replsrvFilePath = "/usr/local/lib/hamler/bin/replsrv",
      hamlerFiles = nullhmfs,
      libBeamPath = "/usr/local/lib/hamler/ebin",
      srcBeamPath = "/usr/local/lib/hamler/ebin",
      coreFilePath = "ebin/$REPL.core"
    }

dictlist :: [FilePath]
dictlist =["ebin","src","test",".deps"]

sourceDirectory :: Opts.Parser FilePath
sourceDirectory = Opts.strOption $
     Opts.short 's'
  <> Opts.long "source"
  <> Opts.value ""
  <> Opts.showDefault
  <> Opts.help "The source directory"

ebinDirectory :: Opts.Parser FilePath
ebinDirectory = Opts.strOption $
     Opts.short 'e'
  <> Opts.long "ebin"
  <> Opts.value "/ebin"
  <> Opts.showDefault
  <> Opts.help "The ebin directory"

commandSrc :: Opts.Parser (IO ())
commandSrc = runRepl <$> sourceDirectory <*> ebinDirectory

runRepl :: FilePath -> FilePath -> IO ()
runRepl sourceDir ebinDir = do
   c <- listDirectory "."
   case all (\i -> i `elem` c) dictlist of
    True -> do
      let srcReplConfig' = srcReplConfig { replsrvFilePath = hamlerFile <> "/bin/replsrv"
                                         , libBeamPath = hamlerFile <> "/ebin"
                                         }
      startReplsrv srcReplConfig' "ebin"
    False -> do
      base <- getTemporaryDirectory
      if sourceDir == "" && ebinDir == "/ebin"
        then do 
          let nullReplConfig' = nullReplConfig { coreFilePath = base </> "hamlerTmp/$REPL.core"
                                           , replsrvFilePath = hamlerFile <> "/bin/replsrv"
                                           , libBeamPath = hamlerFile <> "/ebin"
                                           , srcBeamPath = hamlerFile <> "/ebin"
                                           }
          CE.bracket
            (do 
              let path = base </> "hamlerTmp"
              doesDirectoryExist path >>= \case 
                True -> return ()
                False -> createDirectory path )
            (\_ -> SS.shelly $ SS.run_ "rm" [ "-rf", T.pack $ (base <> "/" <> "hamlerTmp")] )
            (\_ -> startReplsrv nullReplConfig' (base <> "/hamlerTmp"))
        else do 
          let nullReplConfig' = nullReplConfig 
                 { coreFilePath = base </> "hamlerTmp/$REPL.core"
                 , replsrvFilePath = hamlerFile <> "/bin/replsrv"
                 , libBeamPath = hamlerFile <> "/ebin"
                 , srcBeamPath = ebinDir
                 , hamlerFiles = nullhmfs' sourceDir
                 }
          CE.bracket
            (do 
              let path = base </> "hamlerTmp"
              doesDirectoryExist path >>= \case 
                True -> return ()
                False -> createDirectory path )
            (\_ -> SS.shelly $ SS.run_ "rm" [ "-rf", T.pack $ (base <> "/" <> "hamlerTmp")] )
            (\_ -> startReplsrv nullReplConfig' (base <> "/hamlerTmp"))

command :: Opts.Parser (IO ())
command = pure $ startReplsrv devReplConfig "ebin"

startReplsrv :: ReplConfig -> FilePath-> IO ()
startReplsrv ReplConfig {..} fp = do
  fs <- hamlerFiles
  (Just hin, Just hout, Just err, _) <-
    createProcess_
      "start replsrv error!! "
      (proc replsrvFilePath [libBeamPath, srcBeamPath, coreFilePath])
        { std_out = CreatePipe,
          std_in = CreatePipe,
          std_err = CreatePipe
        }
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering
  hSetBuffering err NoBuffering
  _ <- forkIO $ dout hout
  loop hin hout (PSCiOptions fs nodeBackend)
  where
    loop :: Handle -> Handle -> PSCiOptions -> IO ()
    loop hin hout PSCiOptions {..} = do
      inputFiles <- concat <$> traverse (\(x, b) -> fmap (\v -> fmap (\k -> (k,b)) v ) $ glob x) psciInputGlob
      e <- runExceptT $ do
        modules <- ExceptT (loadAllModules' inputFiles)
        when (null modules) . liftIO $ do
          putStr noInputMessage
          exitFailure
        (externs, _) <- ExceptT . runMake . make fp $ fmap (\(a, b, c) -> (a, CST.pureResult b, c)) modules
        return (modules, externs)
      case psciBackend of
        Backend setup _ reload (shutdown :: state -> IO ()) ->
          case e of
            Left errs -> do
              pwd <- getCurrentDirectory
              putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs) >> exitFailure
            Right (modules, externs) -> do
              historyFilename <- getHistoryFilename
              let settings = defaultSettings {historyFile = Just historyFilename}
                  initialState = updateLoadedExterns (const (zip (map (\(_,b,_) -> b) modules) externs)) initialPSCiState
                  config = PSCiConfig (fmap (\(tfp, md, b) -> (P.getModuleName md ,(tfp, b))) modules) fp
                  runner =
                    flip runReaderT config
                      . flip evalStateT initialState
                      . runInputT (setComplete completion settings)
                  handleCommand' :: state -> Command -> StateT PSCiState (ReaderT PSCiConfig IO) ()
                  handleCommand' state = handleCommand (hin, hout) (liftIO (reload state)) (liftIO . putStrLn)
                  go :: state -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                  go state = do
                    v <- lift get
                    c <- getCommand (getVal v)
                    case c of
                      Left err -> outputStrLn err >> go state
                      Right xs -> goExec xs
                    where
                      goExec :: [Command] -> InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                      goExec xs = case xs of
                        [] -> go state
                        (PasteLines : rest) -> do
                          c' <- pasteMode
                          case c' of
                            Left err -> outputStrLn err >> goExec rest
                            Right c'' -> handleCommandWithInterrupts state c'' >> goExec rest
                        (QuitPSCi : _) -> do
                          outputStrLn quitMessage
                          liftIO $ shutdown state
                        (c' : rest) -> handleCommandWithInterrupts state [c'] >> goExec rest
                  handleCommandWithInterrupts ::
                    state ->
                    [Command] ->
                    InputT (StateT PSCiState (ReaderT PSCiConfig IO)) ()
                  handleCommandWithInterrupts state cmds = do
                    handleInterrupt
                      (outputStrLn "Interrupted.")
                      (withInterrupt (lift (for_ cmds (handleCommand' state))))
              putStrLn prologueMessage
              backendState <- setup
              runner (go backendState)
