-- The module is copied from purescript compiler.
module Language.Hamler.Make.Actions
  ( MakeActions(..)
  , RebuildPolicy(..)
  , ProgressMessage(..)
  , buildMakeActions
  ) where

import           Prelude

import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Control.Monad.Supply
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Bifunctor (bimap)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_, minimum)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import qualified Language.JavaScript.Parser as JS
import           Language.PureScript.AST
import qualified Language.PureScript.Bundle as Bundle
import           Language.PureScript.CodeGen.JS.Printer
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreFn.ToJSON as CFJ
import qualified Language.PureScript.CoreImp.AST as Imp
import           Language.PureScript.Crash
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile)
import           Language.PureScript.Make.Monad
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names
import           Language.PureScript.Names (runModuleName, ModuleName)
import           Language.PureScript.Options hiding (codegenTargets)
import           Language.PureScript.Pretty.Common (SMap(..))
import qualified Paths_hamler as Paths
import           SourceMap
import           SourceMap.Types
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>), makeRelative, splitPath, normalise)
import qualified Data.Text.IO as TIO
import           Data.Text (Text, unpack, pack)
import qualified Language.CoreErlang as CE
import           Language.Hamler.CodeGen
import           Language.Hamler.Inline


-- | Determines when to rebuild a module
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways
  deriving (Show, Eq, Ord)

-- | Progress messages from the make process
data ProgressMessage
  = CompilingModule ModuleName
  -- ^ Compilation started for the specified module
  deriving (Show, Eq, Ord)

-- | Render a progress message
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) = "Compiling " ++ T.unpack (runModuleName mn)

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (JavaScript, C++11, etc.)
--
-- * The details of how files are read/written etc.
data MakeActions m = MakeActions
  { getInputTimestampsAndHashes :: ModuleName -> m (Either RebuildPolicy (M.Map FilePath (UTCTime, m ContentHash)))
  -- ^ Get the timestamps and content hashes for the input files for a module.
  -- The content hash is returned as a monadic action so that the file does not
  -- have to be read if it's not necessary.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or 'Nothing' if any of the required
  -- output files are missing.
  , readExterns :: ModuleName -> m (FilePath, Maybe ExternsFile)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , ffiCodegen :: CF.Module CF.Ann -> m ()
  -- ^ Check ffi and print it in the output directory.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
  , readCacheDb :: m CacheDb
  -- ^ Read the cache database (which contains timestamps and hashes for input
  -- files) from some external source, e.g. a file on disk.
  , writeCacheDb :: CacheDb -> m ()
  -- ^ Write the given cache database to some external source (e.g. a file on
  -- disk).
  , outputPrimDocs :: m ()
  -- ^ If generating docs, output the documentation for the Prim modules
  }

-- | A set of make actions that read and write modules from the given directory.
buildMakeActions
  :: FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix =
    MakeActions getInputTimestampsAndHashes getOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb outputPrimDocs
  where

  getInputTimestampsAndHashes
    :: ModuleName
    -> Make (Either RebuildPolicy (M.Map FilePath (UTCTime, Make ContentHash)))
  getInputTimestampsAndHashes mn = do
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    case path of
      Left policy ->
        return (Left policy)
      Right filePath -> do
        let inputPaths = filePath : maybeToList (M.lookup mn foreigns)
            getInfo fp = do
              ts <- getTimestamp fp
              return (ts, hashFile fp)
        pathsWithInfo <- traverse (\fp -> (fp,) <$> getInfo fp) inputPaths
        return $ Right $ M.fromList pathsWithInfo

  outputFilename :: ModuleName -> String -> FilePath
  outputFilename mn fn =
    let filePath = T.unpack (runModuleName mn)
    in outputDir </> filePath </> fn

  targetFilename :: ModuleName -> CodegenTarget -> FilePath
  targetFilename mn = \case
    -- JS -> outputFilename mn ((T.unpack $ runModuleName mn) ++ ".core")
    JS -> outputFilename mn ((T.unpack $ runModuleName mn) ++ ".core")
    JSSourceMap -> outputFilename mn "index.js.map"
    CoreFn -> outputFilename mn "corefn.json"
    Docs -> outputFilename mn "docs.json"

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    codegenTargets <- asks optionsCodegenTargets
    let outputPaths = [outputFilename mn "externs.json"] <> fmap (targetFilename mn) (S.toList codegenTargets)
    timestamps <- traverse getTimestampMaybe outputPaths
    pure $ fmap minimum . NEL.nonEmpty =<< sequence timestamps

  readExterns :: ModuleName -> Make (FilePath, Maybe ExternsFile)
  readExterns mn = do
    let path = outputDir </> T.unpack (runModuleName mn) </> "externs.json"
    (path, ) <$> readExternsFile path

  outputPrimDocs :: Make ()
  outputPrimDocs = do
    codegenTargets <- asks optionsCodegenTargets
    when (S.member Docs codegenTargets) $ for_ Docs.Prim.primModules $ \docsMod@Docs.Module{..} ->
      writeJSONFile (outputFilename modName "docs.json") docsMod

  readModuleInfo :: ModuleName -> SupplyT Make (Text, M.Map Text Int)   -- (Text,[(Text,Int)])
  readModuleInfo mn = do
    let mn' = runModuleName mn
    con <-lift $ makeIO "read module infor" $ TIO.readFile (outputDir </>  (unpack mn' <>  ".info"))
    let list = read (unpack con) :: [(String,Int)]
    return $ (mn', M.fromList $  fmap (\(a,b) -> (pack (unpack mn' <> "." <> a) ,b)) list)


  codegen :: CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT Make ()
  codegen m docs exts = do
    let mn = CF.moduleName m
    efundefs' <- case M.lookup mn foreigns of
      Nothing -> do return []
      Just fp -> do
        con <-lift $ makeIO "read Main.core" $ TIO.readFile fp
        -- let Right (CE.Constr ( CE.Module (CE.Atom ename) eexports _ efundefs )) = fmap inline $ CE.parseModule $ unpack con
        let Right (CE.Constr ( CE.Module (CE.Atom ename) eexports _ efundefs )) =  CE.parseModule $ unpack con
            ff (CE.FunDef (CE.Constr (CE.FunName (CE.Atom n,i))) (CE.Constr expr) ) = (pack $ (unpack $ runModuleName mn) <> "." <> n
                                                                                      , (fromIntegral i,expr))
        return $ fmap ff efundefs
    let mods = filter (/= mn) $  filter (/= ModuleName [ProperName "Prim"]) $  fmap snd $ CF.moduleImports m
    modInfoList <- mapM readModuleInfo mods
    let modInfoMap = M.fromList  modInfoList
    let ((erl,gs),log) = runTranslate modInfoMap efundefs' $  moduleToErl m
    case erl of
      Left e -> lift $ makeIO "print error" $ print e
      Right e@(CE.Module _ exports _ _) -> do
        -- lift $ makeIO "print error" $ putStr $ CE.prettyPrint e
        let mn' = runModuleName mn
        lift $ makeIO "Write core erlang file" $ TIO.writeFile
          (outputDir </>  (unpack mn' <>  (".core")))
          (pack $ CE.prettyPrint e)
        lift $ makeIO "write module information " $ TIO.writeFile
          (outputDir </>  (unpack mn' <>  ".info"))
          (pack $ show $  fmap (\(CE.FunName (CE.Atom s1,i) ) -> (s1,i)) exports )


  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen m = return ()

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage

  readCacheDb :: Make CacheDb
  readCacheDb = fmap (fromMaybe mempty) $ readJSONFile cacheDbFile

  writeCacheDb :: CacheDb -> Make ()
  writeCacheDb = writeJSONFile cacheDbFile

  cacheDbFile = outputDir </> "cache-db.json"


