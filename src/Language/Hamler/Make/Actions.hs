-- The module is copied from purescript compiler.
{-# LANGUAGE TypeApplications #-}
module Language.Hamler.Make.Actions
  ( MakeActions (..),
    RebuildPolicy (..),
    ProgressMessage (..),
    buildMakeActions,
  )
where

import Control.Monad hiding (sequence)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Control.Monad.Supply
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (for_, minimum)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import qualified Language.CoreErlang as CE
import Language.Hamler.CodeGen
import Language.PureScript.AST
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.Crash
import qualified Language.PureScript.Docs.Prim as Docs.Prim
import qualified Language.PureScript.Docs.Types as Docs
import Language.PureScript.Errors
import Language.PureScript.Externs (ExternsFile)
import Language.PureScript.Make hiding (buildMakeActions)
import Language.PureScript.Make.Cache
import Language.PureScript.Names
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Options hiding (codegenTargets)
import System.FilePath ((</>))
import Prelude
import qualified Language.PureScript.Bundle as B
import qualified Language.Hamler.Erlang.CodeGen as E
import qualified Language.Hamler.Erlang.TranslateSimpleType as E
import  Utils as E
import qualified Erlang.Type as E
import qualified Erlang.B as E
import Text.DocLayout (render)
import Control.Carrier.Reader (run, runReader)


-- | Determines when to rebuild a module
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) = "Compiling " ++ T.unpack (runModuleName mn)

-- | A set of make actions that read and write modules from the given directory.
buildMakeActions ::
  HasCallStack =>
  FilePath ->
  Bool ->
  -- | the output directory
  FilePath ->
  -- | a map between module names and paths to the file containing the PureScript module
  M.Map ModuleName (Either RebuildPolicy FilePath) ->
  -- | a map between module name and the file containing the foreign javascript for the module
  M.Map ModuleName FilePath ->
  -- | Generate a prefix comment?
  Bool ->
  MakeActions Make
buildMakeActions libfp isBuildErlSource outputDir filePathMap foreigns _ =
  MakeActions getInputTimestampsAndHashes getOutputTimestamp readExterns codegen ffiCodegen progress readCacheDb writeCacheDb outputPrimDocs
  where
    getInputTimestampsAndHashes ::
      ModuleName ->
      Make (Either RebuildPolicy (M.Map FilePath (UTCTime, Make ContentHash)))
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
       in outputDir </> filePath <> fn

    getFilePath :: HasCallStack => String -> ModuleName -> M.Map ModuleName (Either RebuildPolicy FilePath) -> FilePath
    getFilePath suffix mn tfilePathMap =
      case M.lookup mn tfilePathMap of
        Nothing -> error $ "there is no module name: " ++ show mn ++ show tfilePathMap
        Just v -> case v of
          Left RebuildNever -> libfp </> "ebin" </> T.unpack (runModuleName mn) <> suffix
          _ -> outputDir </> (T.unpack (runModuleName mn) <> suffix)

    getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
    getOutputTimestamp mn = do
      let fp = getFilePath ".json" mn filePathMap
      let outputPaths = [fp] -- <> fmap (targetFilename mn) (S.toList codegenTargets)
      timestamps <- traverse getTimestampMaybe outputPaths
      pure $ fmap minimum . NEL.nonEmpty =<< sequence timestamps

    readExterns :: ModuleName -> Make (FilePath, Maybe ExternsFile)
    readExterns mn = do
      let path = getFilePath ".json" mn filePathMap
      (path,) <$> readExternsFile path

    outputPrimDocs :: Make ()
    outputPrimDocs = do
      codegenTargets <- asks optionsCodegenTargets
      when (S.member Docs codegenTargets) $
        for_ Docs.Prim.primModules $ \docsMod@Docs.Module {..} ->
          writeJSONFile (outputFilename modName "docs.json") docsMod

    readModuleInfo :: HasCallStack => ModuleName -> SupplyT Make (Text, M.Map Text Integer)
    readModuleInfo mn = do
      let mn' = runModuleName mn
          path = getFilePath ".info" mn filePathMap
      con <- lift $ makeIO "read module infor" $ TIO.readFile path
      let list = read (unpack con) :: [(String, Integer)]
      return $ (mn', M.fromList $ fmap (\(a, b) -> (pack (unpack mn' <> "." <> a), b)) list)

    readModuleInfo' :: HasCallStack => ModuleName -> SupplyT Make [(Qualified Ident, Bool)]
    readModuleInfo' mn = do
      let path = getFilePath ".infoErl" mn filePathMap
      con <- lift $ makeIO "read module infor" $ readFile path
      return $ E.toQis con

    codegen :: HasCallStack => CF.Module CF.Ann -> Docs.Module -> ExternsFile -> SupplyT Make ()
    codegen m _ exts = do
      let mn = CF.moduleName m
      lift $ writeJSONFile (outputDir </> (unpack (runModuleName mn) <> ".json")) exts
      ffiModule <- case M.lookup mn foreigns of
        Nothing -> do return Nothing
        Just fp -> do
          con <- lift $ makeIO "read Main.core" $ TIO.readFile fp
          case CE.parseModuleA con of
            Left e -> do
              lift $ throwError $ MultipleErrors [ErrorMessage [] $ ErrorParsingFFIModule fp (Just $ B.UnableToParseModule (show e))]
            Right cemodule -> do
              return $ Just cemodule
      let mods = filter (/= mn) $ filter (/= ModuleName [ProperName "Prim"]) $ fmap snd $ CF.moduleImports m
      modInfoList <- mapM readModuleInfo mods
      let modInfoMap = M.fromList modInfoList
          ((erl, _), _) = runTranslate modInfoMap (ffiModule, mn) $ moduleToErl m 
      case erl of
        Left e -> throwError e
        Right e@(CE.Module _ exports _ _ _) -> do
          let mn' = runModuleName mn
          lift $
            makeIO "Write core erlang file" $
              TIO.writeFile
                (outputDir </> (unpack mn' <> (".core")))
                (CE.prettyText e)
          lift $
            makeIO "write module information " $
              TIO.writeFile
                (outputDir </> (unpack mn' <> ".info"))
                (pack $ show $ fmap (\(CE.FunName (CE.Atom s1 _) i _) -> (s1, i)) exports)

      when isBuildErlSource $ do
          mforms <- case M.lookup mn foreigns of
            Nothing -> do return Nothing
            Just fp -> do
              con <- lift $ makeIO ".core -> .P" $ readFile (Prelude.reverse $ 'P' : drop 4 (Prelude.reverse fp))
              case E.runCalc con of
                E.Failed r -> error r
                E.OK fs -> return $ Just fs

          let getFFiDecArgs :: Maybe E.Forms -> [(Qualified Ident, Int)]
              getFFiDecArgs Nothing = []
              getFFiDecArgs (Just fs) =
                let ls = E.formsToList fs
                    getDec :: E.Form -> [(String, Integer)]
                    getDec (E.ExportList l) = E.dec l
                    getDec _ = []
                 in map (\(s, i) -> (Qualified (Just mn) (Ident (pack s)), fromIntegral i)) $ concatMap getDec ls
              ffiM = M.fromList $ getFFiDecArgs mforms

          let mods = filter (/= mn) $ filter (/= ModuleName [ProperName "Prim"]) $ fmap snd $ CF.moduleImports m

          modInfoList <- concat <$> mapM readModuleInfo' mods
          let otherM = M.fromList modInfoList

              forms = run $ runReader (E.createTenv m otherM ffiM) (E.rModule m)
              exports = run $ runReader (E.createTenv m otherM ffiM) (E.rExport m)

          lift $
            makeIO "write module information " $
              writeFile
                (outputDir </> (unpack (runModuleName mn) <> ".infoErl"))
                (E.toStrings exports)

          lift $
            makeIO "Write core erlang file" $ do
              writeFile
                (outputDir </> (unpack (runModuleName mn ) <> ".erl"))
                (render @String Nothing $ pretty forms)



    ffiCodegen :: CF.Module CF.Ann -> Make ()
    ffiCodegen _ = return ()

    progress :: ProgressMessage -> Make ()
    progress = liftIO . putStrLn . renderProgressMessage

    readCacheDb :: Make CacheDb
    readCacheDb = fmap (fromMaybe mempty) $ readJSONFile cacheDbFile

    writeCacheDb :: CacheDb -> Make ()
    writeCacheDb = writeJSONFile cacheDbFile

    cacheDbFile = outputDir </> "cache-db.json"
