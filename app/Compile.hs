{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compile (command,initProject,buildlib,runProject) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import           Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import           Language.PureScript.Errors.JSON
import qualified Options.Applicative as Opts
import qualified System.Console.ANSI as ANSI
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.FilePath.Glob (glob)
import           System.IO (hPutStr, hPutStrLn, stderr)
import           System.IO.UTF8 (readUTF8FilesT)
import           System.Directory
import           Language.Hamler.Make
import qualified Shelly as SS
import qualified Data.List as LL
-- import           Prelude

data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [FilePath]
  , pscmOutputDir    :: FilePath
  , pscmOpts         :: P.Options
  , pscmUsePrefix    :: Bool
  , pscmJSONErrors   :: Bool
  }

-- | Arguments: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: PSCMakeOptions -> IO ()
compile PSCMakeOptions{..} = do
  input <- globWarningOnMisses (unless pscmJSONErrors . warnFileTypeNotFound) pscmInput
  when (null input && not pscmJSONErrors) $ do
    hPutStr stderr $ unlines [ "hamler compile: No input files."
                             , "Usage: For basic information, try the `--help' option."
                             ]
    exitFailure
  moduleFiles <- readUTF8FilesT input
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    -- P.make makeActions (map snd ms)
    make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
  -- exitSuccess

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("hamler compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [FilePath] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning pattern' = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths
  concatMapM f = fmap concat . mapM f

inputFile :: Opts.Parser FilePath
inputFile = Opts.strArgument $
     Opts.metavar "FILE"
  <> Opts.help "The input .purs file(s)."

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "output"
  <> Opts.showDefault
  <> Opts.help "The output directory"

comments :: Opts.Parser Bool
comments = Opts.switch $
     Opts.short 'c'
  <> Opts.long "comments"
  <> Opts.help "Include comments in the generated code"

verboseErrors :: Opts.Parser Bool
verboseErrors = Opts.switch $
     Opts.short 'v'
  <> Opts.long "verbose-errors"
  <> Opts.help "Display verbose error messages"

noPrefix :: Opts.Parser Bool
noPrefix = Opts.switch $
     Opts.short 'p'
  <> Opts.long "no-prefix"
  <> Opts.help "Do not include comment header"

jsonErrors :: Opts.Parser Bool
jsonErrors = Opts.switch $
     Opts.long "json-errors"
  <> Opts.help "Print errors to stderr as JSON"

codegenTargets :: Opts.Parser [P.CodegenTarget]
codegenTargets = Opts.option targetParser $
     Opts.short 'g'
  <> Opts.long "codegen"
  <> Opts.value [P.JS]
  <> Opts.help
      ( "Specifies comma-separated codegen targets to include. "
      <> targetsMessage
      <> " The default target is 'js', but if this option is used only the targets specified will be used."
      )

targetsMessage :: String
targetsMessage = "Accepted codegen targets are '" <> intercalate "', '" (M.keys P.codegenTargets) <> "'."

targetParser :: Opts.ReadM [P.CodegenTarget]
targetParser =
  Opts.str >>= \s ->
    for (T.split (== ',') s)
      $ maybe (Opts.readerError targetsMessage) pure
      . flip M.lookup P.codegenTargets
      . T.unpack
      . T.strip

options :: Opts.Parser P.Options
options =
  P.Options
    <$> verboseErrors
    <*> (not <$> comments)
    <*> (handleTargets <$> codegenTargets)
  where
    -- Ensure that the JS target is included if sourcemaps are
    handleTargets :: [P.CodegenTarget] -> S.Set P.CodegenTarget
    handleTargets ts = S.fromList (if elem P.JSSourceMap ts then P.JS : ts else ts)

pscMakeOptions :: Opts.Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)
                                <*> jsonErrors


command :: Opts.Parser (IO ())
command = pure $ do
  dir <- getCurrentDirectory
  isExist <- doesDirectoryExist hamlerlib
  fps1 <- if isExist
          then gethmFiles hamlerlib
          else gethmFiles (dir <> "/.deps/hamler/lib")
  fps2 <- gethmFiles (dir <> "/src")
  let fps = fps1 <> fps2
      fps2' = fmap hmToCore fps2
  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = dir <>  "/ebin"
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          }
          )
  let tpath = dir <> "/ebin"
  cfs <- findFile1 ".core" tpath
  forM_ (filter (`elem` fps2') cfs) $ \fp -> do
    SS.shelly $ SS.command "erlc" ["-o" ,T.pack tpath] [T.pack $ tpath <> "/" <> fp]
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  ifs <- findFile1 ".info" tpath
  forM_ ifs $ \fp -> do
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]
    return ()

  ifs <- findFile1 ".core" tpath
  forM_ ifs $ \fp -> do
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]
    return ()


  exitSuccess

hmToCore :: FilePath -> FilePath
hmToCore fp =reverse $ concat $ LL.intersperse "." $ LL.takeWhile (/="crs") $ words $ fmap cc $  ("eroc" <> (drop 2 $ reverse fp))

cc :: Char -> Char
cc ('/')= ' '
cc x = x


-- build lib
buildlib :: Opts.Parser (IO ())
buildlib = pure $ do
  dir <- getCurrentDirectory
  isExist <- doesDirectoryExist hamlerlib
  fps1 <- if isExist
          then gethmFiles hamlerlib
          else gethmFiles (dir <> "/.deps/hamler/lib")
  let fps = fps1
      tpath = dir <> "/ebin"
  removeDirectoryRecursive tpath
  createDirectory tpath
  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = dir <>  "/ebin"
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          }
          )
  cfs <- findFile1 ".core" tpath
  forM_ cfs $ \fp -> do
    SS.shelly $ SS.command "erlc" ["-o" ,T.pack tpath] [T.pack $ tpath <> "/" <> fp]
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  ifs <- findFile1 ".info" tpath
  forM_ ifs $ \fp -> do
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]
  exitSuccess




runProject :: Opts.Parser (IO ())
runProject  =pure $ do
  dir <- getCurrentDirectory
  let tpath = dir <> "/ebin"
  SS.shelly $ SS.run "erl" ["-pa",T.pack (tpath), "-noshell","-s" ,"Main","main","-s","init","stop" ]
  return ()



-- | isFile  ".core" "Main.core"   -> True
isFile :: String -> String -> Bool
isFile st fp= let l = length st
                  s = take l $ reverse fp
              in reverse st == s


findFile1 :: String -> FilePath -> IO [FilePath]
findFile1 base fp = do
  fs <- listDirectory fp
  return $ filter (isFile base) fs


dictlist :: [FilePath]
dictlist =["ebin","src","test",".deps"]

helloHamler :: String
helloHamler = concat [
          "module Main where\n"
        , "\n"
        , "import System.IO\n"
        , "\n"
        , "main :: String\n"
        , "main = print "
        , "\"Let there be Hamler, running on Erlang VM!\"\n"
        ]

makeFile :: String
makeFile = concat [ ".PHONY : build run\n"
                  , "all: build\n"
                  , "build:\n"
                  , "\thamler build\n"
                  , "run:\n"
                  , "\thamler run\n"
                  ]



liblink = "https://github.com/hamler-lang/hamler.git"

hamlerlib = "/usr/local/lib/hamler/lib"

initProject :: Opts.Parser (IO ())
initProject  =pure $ do
  base <- getCurrentDirectory
  let dictlist' = fmap (\x -> base <> "/" <> x) dictlist
  mapM createDirectory dictlist'
  writeFile "src/Main.hm" helloHamler
  writeFile "Makefile" makeFile
  isExist <- doesDirectoryExist hamlerlib
  if isExist
    then return ()
    else do
       SS.shelly $ SS.run "git" ["clone",liblink,".deps/hamler"] 
       return ()
  print "hamler init finish!"


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
      else if ishmFile filePath
              then return [tp]
                   else return []
  return $ concat r

















