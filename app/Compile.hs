{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compile (command,initProject,runProject) where

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
  , isInline         :: Bool
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
    let makeActions = buildMakeActions isInline pscmOutputDir filePathMap foreigns pscmUsePrefix
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
                                <*> (pure False)


howBuild :: Opts.Parser Bool
howBuild= Opts.switch $
     Opts.short 'l'
  <> Opts.long "libraries"
  <> Opts.help "build the libraries to ebin"


inline :: Opts.Parser Bool
inline= Opts.switch $
     Opts.short 'i'
  <> Opts.long "inline"
  <> Opts.help "Determine whether to inline functions when reading .core"



command :: Opts.Parser (IO ())
command = buildFun <$> inline
                   <*> howBuild

buildFun ::Bool -> Bool -> IO ()
buildFun isIn b = if b
                  then buildlib isIn
                  else buildSrc isIn

buildSrc :: Bool -> IO ()
buildSrc bool = do
  print bool
  dir <- getCurrentDirectory
  isExist <- doesDirectoryExist hamlerlib
  fps1 <- if isExist
          then gethmFiles hamlerlib
          else gethmFiles (dir <> "/.deps/hamler/lib")
  fps2 <- gethmFiles (dir <> "/src")
  let fps = fps1 <> fps2
      fps2' = fmap hmToCore fps2
  let tpath = dir <> "/ebin"
  removeDirectoryRecursive tpath
  createDirectory tpath
  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = dir <>  "/ebin"
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          , isInline       = bool
                          }
          )
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
buildlib :: Bool -> IO ()
buildlib bool = do
  print bool
  dir <- getCurrentDirectory
  fps1 <- gethmFiles (dir <> "/lib")
  let fps = fps1
      tpath = dir <> "/ebin"
  r <- doesDirectoryExist tpath
  if r
    then return ()
    else createDirectory tpath
  list <- findFile1 ".beam" tpath
  forM_ list $ \fp -> do
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = dir <>  "/ebin"
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          , isInline       = bool
                          }
          )
  cfs <- findFile1 ".core" tpath
  forM_ cfs $ \fp -> do
    SS.shelly $ SS.command "erlc" ["-o" ,T.pack tpath] [T.pack $ tpath <> "/" <> fp]
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  ifs <- findFile1 ".info" tpath
  forM_ ifs $ \fp -> do
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  jfs <- findFile1 ".json" tpath
  forM_ jfs $ \fp -> do
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  exitSuccess

runProject :: Opts.Parser (IO ())
runProject  =pure $ do
  dir <- getCurrentDirectory
  let tpath = dir <> "/ebin"
  isExist <- doesDirectoryExist hamlerlib
  SS.shelly $ do
    if isExist
      then SS.setenv "ERL_LIBS" "/usr/local/lib/hamler/"
      else SS.setenv "ERL_LIBS" (T.pack $ dir <> ".deps/hamler")
    SS.run  "erl" ["-pa",T.pack (tpath), "-noshell","-s" ,"Main","main","-s","init","stop" ]
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
        , "import Prelude\n"
        , "\n"
        , "main :: IO String\n"
        , "main = print "
        , "\"Let there be Hamler, running on Erlang VM!\"\n"
        ]

makeFile :: String
makeFile = concat [ ".PHONY : build run\n\n"
                  , "all: build\n\n"
                  , "build:\n"
                  , "\t@hamler build\n\n"
                  , "run:\n"
                  , "\t@hamler run\n"
                  ]

liblink = "https://github.com/hamler-lang/hamler.git"

hamlerlib = "/usr/local/lib/hamler/lib"

initProject :: Opts.Parser (IO ())
initProject = pure $ do
  base <- getCurrentDirectory
  let dictlist' = fmap (\x -> base <> "/" <> x) dictlist
  mapM createDirectory dictlist'
  putStrLn "Generating src/Main.hm..."
  writeFile "src/Main.hm" helloHamler
  putStrLn "Generating Makefile..."
  writeFile "Makefile" makeFile
  isExist <- doesDirectoryExist hamlerlib
  if isExist
    then do
       return ()
    else do
       SS.shelly $ SS.run "git" ["clone",liblink,".deps/hamler"]
       return ()

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

