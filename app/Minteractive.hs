{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Minteractive
  ( handleCommand,
    module Interactive,
    make,
    runMake,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT)
import Control.Monad.Writer.Strict (Writer (), runWriter)
import Data.List (find, foldl', sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.Hamler.Make as H
import Language.Hamler.Make (inferForeignModules)
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Constants as C
import Language.PureScript.Interactive.Completion as Interactive
import Language.PureScript.Interactive.IO as Interactive
import Language.PureScript.Interactive.Message as Interactive
import Language.PureScript.Interactive.Module as Interactive
import Language.PureScript.Interactive.Parser as Interactive
import Language.PureScript.Interactive.Printer as Interactive
import Language.PureScript.Interactive.Types as Interactive
import qualified Language.PureScript.Names as N
import Prelude.Compat
import Protolude (ordNub)
import System.Directory (getCurrentDirectory)
import System.FilePath.Glob (glob)
import Prelude ()
import System.IO (Handle, hPutStrLn)
import Control.Concurrent
import Version (hamlerEnv)


hamlerFile :: FilePath
hamlerFile = $hamlerEnv

-- | Pretty-print errors
printErrors :: MonadIO m => P.MultipleErrors -> m ()
printErrors errs = liftIO $ do
  pwd <- getCurrentDirectory
  putStrLn $ P.prettyPrintMultipleErrors P.defaultPPEOptions {P.ppeRelativeDirectory = pwd} errs

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fst <$> P.runMake P.defaultOptions mk

-- | Rebuild a module, using the cached externs data for dependencies.
rebuild :: [(P.ModuleName, (FilePath, Bool))] -> 
  FilePath ->
  [P.ExternsFile] ->
  P.Module ->
  P.Make (P.ExternsFile, P.Environment)
rebuild conf fp loadedExterns m = do
  externs <- P.rebuildModule buildActions loadedExterns m
  return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment (loadedExterns ++ [externs]))
  where
    buildActions :: P.MakeActions P.Make
    buildActions =
      ( H.buildMakeActions
          hamlerFile
          False
          fp
          filePathMap
          M.empty
          False
      )
        { P.progress = const (return ())
        }
    decPolic x mfp = case x of 
                    True -> Left P.RebuildNever
                    False -> Right mfp
    fls = fmap (\(a,(b,c)) -> (a, decPolic c b) ) conf
    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.fromList $ ((P.getModuleName m),(Left P.RebuildAlways)) :fls

-- | Build the collection of modules from scratch. This is usually done on startup.
make :: FilePath ->
  [(FilePath, CST.PartialResult P.Module,Bool)] ->
  P.Make ([P.ExternsFile], P.Environment)
make fp1 ms = do
  foreignFiles <- inferForeignModules filePathMap
  externs <- P.make (buildActions foreignFiles) (map (\(_,b,_) -> b) ms)
  return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs)
  where
    buildActions :: M.Map P.ModuleName FilePath -> P.MakeActions P.Make
    buildActions foreignFiles =
      H.buildMakeActions
        hamlerFile 
        False
        fp1
        filePathMap
        foreignFiles
        False
    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.fromList $ map (\(fp, m,c) -> case c of 
                                                    False -> (P.getModuleName $ CST.resPartial m, Right fp)
                                                    True -> (P.getModuleName $ CST.resPartial m, Left H.RebuildNever)
                                   ) ms

-- | Performs a PSCi command
handleCommand ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  -- | evaluate JS
  (Handle,Handle) ->
  -- | reload
  m () ->
  -- | print into console
  (String -> m ()) ->
  Command ->
  m ()
handleCommand _ _ p ShowHelp = p helpMessage
handleCommand _ r _ ReloadState = handleReloadState r
handleCommand _ r _ ClearState = handleClearState r
handleCommand e _ _ (Expression val) = handleExpression e val
handleCommand _ _ _ (Import im) = handleImport im
handleCommand _ _ _ (Decls l) = handleDecls l
handleCommand _ _ p (TypeOf val) = handleTypeOf p val
handleCommand _ _ p (KindOf typ) = handleKindOf p typ
handleCommand _ _ p (BrowseModule moduleName) = handleBrowse p moduleName
handleCommand _ _ p (ShowInfo QueryLoaded) = handleShowLoadedModules p
handleCommand _ _ p (ShowInfo QueryImport) = handleShowImportedModules p
handleCommand _ _ p (ShowInfo QueryPrint) = handleShowPrint p
handleCommand _ _ p (CompleteStr prefix) = handleComplete p prefix
handleCommand _ _ p (SetInteractivePrint ip) = handleSetInteractivePrint p ip
handleCommand _ _ _ (Setval _ s2)  = handleSetVal s2
handleCommand _ _ _ _ = P.internalError "handleCommand: unexpected command"

-- | Reload the application state
handleReloadState ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  m () ->
  m ()
handleReloadState reload = do
  modify $ updateLets (const [])
  globs <- asks psciFileGlobs
  dirs <- asks moduleDirs
  files <- liftIO $ concat <$> traverse (\(f,b) -> (fmap (\x -> (x,b) )) <$> glob f ) (fmap snd globs)
  e <- runExceptT $ do
    modules <- ExceptT . liftIO $ loadAllModules' files
    (externs, _) <- ExceptT . liftIO . runMake . make dirs $ fmap (\(a,b,c) -> (a, CST.pureResult b, c)) modules
    return (map (\(_, b, _) -> b) modules, externs)
  case e of
    Left errs -> printErrors errs
    Right (modules, externs) -> do
      modify (updateLoadedExterns (const (zip modules externs)))
      reload

-- | Clear the application state
handleClearState ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  m () ->
  m ()
handleClearState reload = do
  modify $ updateImportedModules (const [])
  handleReloadState reload

-- | Takes a value expression and evaluates it with the current state.
handleExpression ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  (Handle,Handle) ->
  P.Expr ->
  m ()
handleExpression (hin, _) val = do
  st <- get
  let m = createTemporaryModule True st val
  dirs <- asks moduleDirs
  conf <- asks psciFileGlobs
  e <- liftIO . runMake $ rebuild conf dirs (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right _ -> do
      liftIO $ hPutStrLn hin "start"
      -- l <- liftIO $ hGetLine hout
      -- liftIO $ putStrLn l
      liftIO $ threadDelay (10000 * 5)
      return ()

-- |
-- Takes a list of declarations and updates the environment, then run a make. If the declaration fails,
-- restore the original environment.
handleDecls ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  [P.Declaration] ->
  m ()
handleDecls ds = do
  st <- gets (updateLets (++ ds))
  let m = createTemporaryModule False st (P.Literal P.nullSourceSpan (P.ObjectLiteral []))
  dirs <- asks moduleDirs
  conf <- asks psciFileGlobs
  e <- liftIO . runMake $ rebuild conf dirs (map snd (psciLoadedExterns st)) m
  case e of
    Left err -> printErrors err
    Right _ -> put st


-- | Show actual loaded modules in psci.
handleShowLoadedModules ::
  (MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  m ()
handleShowLoadedModules print' = do
  loadedModules <- gets psciLoadedExterns
  print' $ readModules loadedModules
  where
    readModules = unlines . sort . ordNub . map (T.unpack . P.runModuleName . P.getModuleName . fst)

-- | Show the imported modules in psci.
handleShowImportedModules ::
  (MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  m ()
handleShowImportedModules print' = do
  importedModules <- psciImportedModules <$> get
  print' $ showModules importedModules
  where
    showModules = unlines . sort . map (T.unpack . showModule)
    showModule (mn, declType, asQ) =
      "import " <> N.runModuleName mn <> showDeclType declType
        <> foldMap (\mn' -> " as " <> N.runModuleName mn') asQ
    showDeclType P.Implicit = ""
    showDeclType (P.Explicit refs) = refsList refs
    showDeclType (P.Hiding refs) = " hiding " <> refsList refs
    refsList refs = " (" <> commaList (mapMaybe showRef refs) <> ")"
    showRef :: P.DeclarationRef -> Maybe Text
    showRef (P.TypeRef _ pn dctors) =
      Just $ N.runProperName pn <> "(" <> maybe ".." (commaList . map N.runProperName) dctors <> ")"
    showRef (P.TypeOpRef _ op) =
      Just $ "type " <> N.showOp op
    showRef (P.ValueRef _ ident) =
      Just $ N.runIdent ident
    showRef (P.ValueOpRef _ op) =
      Just $ N.showOp op
    showRef (P.TypeClassRef _ pn) =
      Just $ "class " <> N.runProperName pn
    showRef (P.TypeInstanceRef _ ident) =
      Just $ N.runIdent ident
    showRef (P.ModuleRef _ name) =
      Just $ "module " <> N.runModuleName name
    showRef (P.KindRef _ pn) =
      Just $ "kind " <> N.runProperName pn
    showRef (P.ReExportRef _ _ _) =
      Nothing
    commaList :: [Text] -> Text
    commaList = T.intercalate ", "

handleShowPrint ::
  (MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  m ()
handleShowPrint print' = do
  current <- psciInteractivePrint <$> get
  if current == initialInteractivePrint
    then
      print' $
        "The interactive print function is currently set to the default (`" ++ showPrint current ++ "`)"
    else
      print' $
        "The interactive print function is currently set to `" ++ showPrint current ++ "`\n"
          ++ "The default can be restored with `:print "
          ++ showPrint initialInteractivePrint
          ++ "`"
  where
    showPrint (mn, ident) = T.unpack (N.runModuleName mn <> "." <> N.runIdent ident)

-- | Imports a module, preserving the initial state on failure.
handleImport ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  ImportedModule ->
  m ()
handleImport im = do
  st <- gets (updateImportedModules (im :))
  let m = createTemporaryModuleForImports st
  dirs <- asks moduleDirs
  conf <- asks psciFileGlobs
  e <- liftIO . runMake $ rebuild conf dirs (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right _ -> put st

-- | Takes a value and prints its type
handleTypeOf ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  P.Expr ->
  m ()
handleTypeOf print' val = do
  st <- get
  let m = createTemporaryModule False st val
  dirs <- asks moduleDirs
  conf <- asks psciFileGlobs
  e <- liftIO . runMake $ rebuild conf dirs (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case M.lookup (P.mkQualified (P.Ident "it") (P.ModuleName [P.ProperName "$REPL"])) (P.names env') of
        Just (ty, _, _) -> print' . P.prettyPrintType maxBound $ ty
        Nothing -> print' "Could not find type"

-- | Takes a type and prints its kind
handleKindOf ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  P.SourceType ->
  m ()
handleKindOf print' typ = do
  st <- get
  let m = createTemporaryModuleForKind st typ
      mName = P.ModuleName [P.ProperName "$REPL"]
  dirs <- asks moduleDirs
  conf <- asks psciFileGlobs
  e <- liftIO . runMake $ rebuild conf dirs (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = (P.emptyCheckState env') {P.checkCurrentModule = Just mName}
              k = check (P.kindOf typ') chk
              check :: StateT P.CheckState (ExceptT P.MultipleErrors (Writer P.MultipleErrors)) a -> P.CheckState -> Either P.MultipleErrors (a, P.CheckState)
              check sew = fst . runWriter . runExceptT . runStateT sew
          case k of
            Left err -> printErrors err
            Right (kind, _) -> print' . T.unpack . P.prettyPrintKind $ kind
        Nothing -> print' "Could not find kind"

-- | Browse a module and displays its signature
handleBrowse ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  P.ModuleName ->
  m ()
handleBrowse print' moduleName = do
  st <- get
  let env = psciEnvironment st
  case findMod moduleName (psciLoadedExterns st) (psciImportedModules st) of
    Just qualName -> print' $ printModuleSignatures qualName env
    Nothing -> failNotInEnv moduleName
  where
    findMod needle externs imports =
      let qualMod = fromMaybe needle (lookupUnQualifiedModName needle imports)
          modules = S.fromList (C.primModules <> (P.getModuleName . fst <$> externs))
       in if qualMod `S.member` modules
            then Just qualMod
            else Nothing
    failNotInEnv modName = print' $ T.unpack $ "Module '" <> N.runModuleName modName <> "' is not valid."
    lookupUnQualifiedModName needle imports =
      (\(modName, _, _) -> modName) <$> find (\(_, _, mayQuaName) -> mayQuaName == Just needle) imports

-- | Return output as would be returned by tab completion, for tools integration etc.
handleComplete ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  String ->
  m ()
handleComplete print' prefix = do
  st <- get
  let act = liftCompletionM (completion' (reverse prefix, ""))
  results <- evalStateT act st
  print' $ unlines (formatCompletions results)

-- | Attempt to set the interactive print function. Note that the state will
-- only be updated if the interactive print function exists and appears to
-- work; we test it by attempting to evaluate '0'.
handleSetInteractivePrint ::
  (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m) =>
  (String -> m ()) ->
  (P.ModuleName, P.Ident) ->
  m ()
handleSetInteractivePrint print' new = do
  current <- gets psciInteractivePrint
  modify (setInteractivePrint new)
  st <- get
  let expr = P.Literal internalSpan (P.NumericLiteral (Left 0))
  let m = createTemporaryModule True st expr
  dirs <- asks moduleDirs
  conf <- asks psciFileGlobs
  e <- liftIO . runMake $ rebuild conf dirs (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> do
      modify (setInteractivePrint current)
      print' "Unable to set the repl's printing function:"
      printErrors errs
    Right _ ->
      pure ()

handleSetVal ::
  (MonadState PSCiState m, MonadIO m) =>
  String -> m ()
handleSetVal s = do
  modify (setVal (s ,s))
