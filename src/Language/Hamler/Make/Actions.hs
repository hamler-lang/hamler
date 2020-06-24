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
import           Data.Foldable (for_, minimum)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.List as L
import           Data.Time.Clock (UTCTime)
import           Language.PureScript.AST
import qualified Language.PureScript.CoreFn as CF
import           Language.PureScript.Crash
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile)
import           Language.PureScript.Make.Monad
import           Language.PureScript.Make.Cache
import           Language.PureScript.Names
import           Language.PureScript.Names (runModuleName, ModuleName)
import           Language.PureScript.Options hiding (codegenTargets)
import           System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import           Data.Text (Text, unpack, pack)
import qualified Language.CoreErlang as CE
import           Language.Hamler.CodeGen
import           Language.PureScript.Make hiding (buildMakeActions)

-- | Determines when to rebuild a module
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) = "Compiling " ++ T.unpack (runModuleName mn)
-- | A set of make actions that read and write modules from the given directory.
buildMakeActions
  :: Bool
  -> FilePath
  -- ^ the output directory
  -> M.Map ModuleName (Either RebuildPolicy FilePath)
  -- ^ a map between module names and paths to the file containing the PureScript module
  -> M.Map ModuleName FilePath
  -- ^ a map between module name and the file containing the foreign javascript for the module
  -> Bool
  -- ^ Generate a prefix comment?
  -> MakeActions Make
buildMakeActions isInline outputDir filePathMap foreigns _ =
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
              ts1 <- getTimestamp fp
              return (ts1, hashFile fp)
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
    let path = outputDir </> (T.unpack (runModuleName mn) <> ".json")
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
  codegen m _ exts = do
    let mn = CF.moduleName m
    lift $ writeJSONFile (outputDir </> ( unpack (runModuleName mn) <> ".json")) exts
    (efundefs',rpart) <- case M.lookup mn foreigns of
      Nothing -> do return ([],Nothing)
      Just fp -> do
        con' <-lift $ makeIO "read Main.core" $ TIO.readFile fp
        let (con,respart) = splitErlangCore con'
        case CE.parseModuleHead $ unpack con of
         Left e -> do
           lift $ makeIO "read Main.core" $ print ("error of parse core file: ---> " <> fp)
           lift $ throwError $ MultipleErrors [ErrorMessage [] $ ErrorParsingModule e]
         Right (CE.Constr ( CE.ModuleHead (CE.Atom _) epos _ )) -> do
            let ff (CE.FunName (CE.Atom n,i))
                  = (pack $ (unpack $ runModuleName mn) <> "." <> n, (fromIntegral i))
            return $ (fmap ff epos, Just respart)
         x -> error $ show x
    let mods = filter (/= mn) $  filter (/= ModuleName [ProperName "Prim"]) $  fmap snd $ CF.moduleImports m
    modInfoList <- mapM readModuleInfo mods
    let modInfoMap = M.fromList modInfoList
        ((erl,_),_) = runTranslate isInline modInfoMap efundefs' $  moduleToErl m
    case erl of
      Left e -> lift $ makeIO "print error" $ print e
      Right e@(CE.Module _ exports _ _) -> do
        -- lift $ makeIO "print error" $ putStr $ CE.prettyPrint e
        let mn' = runModuleName mn
        lift $ makeIO "Write core erlang file" $ TIO.writeFile
          (outputDir </>  (unpack mn' <>  (".core")))
          ( case rpart of
              Nothing    -> pack $ CE.prettyPrint e
              Just rep -> (T.unlines . L.init . T.lines . pack $ CE.prettyPrint e) <> rep
          )
        lift $ makeIO "write module information " $ TIO.writeFile
          (outputDir </>  (unpack mn' <>  ".info"))
          (pack $ show $  fmap (\(CE.FunName (CE.Atom s1,i) ) -> (s1,i)) exports )


  ffiCodegen :: CF.Module CF.Ann -> Make ()
  ffiCodegen _ = return ()

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage

  readCacheDb :: Make CacheDb
  readCacheDb = fmap (fromMaybe mempty) $ readJSONFile cacheDbFile

  writeCacheDb :: CacheDb -> Make ()
  writeCacheDb = writeJSONFile cacheDbFile

  cacheDbFile = outputDir </> "cache-db.json"

splitErlangCore :: T.Text -> (T.Text,T.Text)
splitErlangCore f =
  let (a,b) = L.span (\c -> T.head c /= '\'') $ T.lines f
      (c,_) = L.span ( /= "\'module_info\'/0 =" ) b
  in (T.unlines a , T.unlines $ c <> ["end"])