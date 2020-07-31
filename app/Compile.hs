{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Compile (command, initProject, runProject, buildTest) where

import           Control.Monad
import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
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
import           Control.Concurrent.Async.Lifted
import Prelude
import Version (hamlerEnv)

data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [(FilePath, Bool)]
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
  cc0 <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc0, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
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
    let filePathMap = M.fromList $ map (\(fp, pm) -> (P.getModuleName $ CST.resPartial pm, inpRebPol fp pscmInput) ) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions hamlerFile isInline pscmOutputDir filePathMap foreigns pscmUsePrefix
    -- P.make makeActions (map snd ms)
    make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
  -- exitSuccess

inpRebPol :: FilePath -> [(FilePath, Bool)] -> Either RebuildPolicy FilePath
inpRebPol fp xs = case LL.lookup fp xs of 
                    Nothing -> error "strange error"
                    Just b -> if b
                              then Left RebuildNever 
                              else Right fp

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("hamler compile: No files found using pattern: " ++)

globWarningOnMisses :: (String -> IO ()) -> [(FilePath, Bool)] -> IO [FilePath]
globWarningOnMisses warn = concatMapM globWithWarning
  where
  globWithWarning (pattern', _) = do
    paths <- glob pattern'
    when (null paths) $ warn pattern'
    return paths
  concatMapM f = fmap concat . mapM f

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

outputDirectory :: Opts.Parser FilePath
outputDirectory = Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output"
  <> Opts.value "/ebin"
  <> Opts.showDefault
  <> Opts.help "The output directory"

command :: Opts.Parser (IO ())
command = buildFun <$> inline
                   <*> howBuild
                   <*> outputDirectory

buildFun ::Bool -> Bool -> FilePath -> IO ()
buildFun isIn b fp = if b
                  then buildlib isIn
                  else buildSrc isIn fp

buildSrc :: Bool -> FilePath -> IO ()
buildSrc bl fpath = do
  dir <- getCurrentDirectory
  isExist <- doesDirectoryExist hamlerlib
  fps1 <- if isExist
          then gethmFiles hamlerlib
          else gethmFiles (dir <> "/.deps/hamler/lib")
  fps2 <- gethmFiles (dir <> "/src")
  let fps = fmap (\v -> (v, True)) fps1 <> fmap (\v -> (v, False)) fps2
      fps2' = fmap hmToCore fps2
  let tpath = if fpath == "/ebin"  
              then dir <> "/ebin"
              else fpath
  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = tpath
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          , isInline       = bl
                          }
          )
  cfs <- findFile1 ".core" tpath
  forM_ (filter (`elem` fps2') cfs) $ \fp -> do
    SS.shelly $ SS.command_ "erlc" ["-o" ,T.pack tpath] [T.pack $ tpath <> "/" <> fp]
    SS.shelly $ SS.run "rm" [T.pack $ tpath <> "/" <> fp]

  exitSuccess

hmToCore :: FilePath -> FilePath
hmToCore fp =reverse $ concat $ LL.intersperse "." $ LL.takeWhile (/="crs") $ words $ fmap cc $  ("eroc" <> (drop 2 $ reverse fp))

cc :: Char -> Char
cc ('/')= ' '
cc x = x

-- build lib
buildlib :: Bool -> IO ()
buildlib bl = do
  dir <- getCurrentDirectory
  fps1 <- gethmFiles (dir <> "/lib")
  let fps = fmap (\v -> (v, False)) fps1
      tpath = dir <> "/ebin"
  r <- doesDirectoryExist tpath
  if r
    then return ()
    else createDirectory tpath

  recErlc (dir <> "/lib")

  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = dir <>  "/ebin"
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          , isInline       = bl
                          }
          )
  cfs <- findFile1 ".core" tpath

  SS.shelly $ SS.command_ "erlc" ["-o" ,T.pack tpath] (fmap (\fp -> T.pack $ tpath <> "/" <> fp) cfs)

  forM_ cfs $ \fp -> do
    SS.shelly $ SS.run_ "rm" [T.pack $ tpath <> "/" <> fp]

  exitSuccess

buildTest :: Opts.Parser (IO ())
buildTest = pure $ do
  dir <- getCurrentDirectory
  fps1 <- gethmFiles (dir <> "/lib")
  fps2 <- gethmFiles (dir <> "/tests")
  let fps = fmap (\v -> (v, False)) (fps1 <> fps2)
      tpath = dir <> "/.test"
  r <- doesDirectoryExist tpath
  if r
    then return ()
    else createDirectory tpath

  recErlc (dir <> "/lib")

  compile (PSCMakeOptions { pscmInput      = fps
                          , pscmOutputDir  = dir <>  "/.test"
                          , pscmOpts       = (P.Options False False (S.fromList [P.CoreFn]))
                          , pscmUsePrefix  = False
                          , pscmJSONErrors = False
                          , isInline       = False
                          }
          )
  cfs <- findFile1 ".core" tpath
  SS.shelly $ SS.command_ "erlc" ["-o" ,T.pack tpath] (fmap (\fp -> T.pack $ tpath <> "/" <> fp) cfs)
  forM_ cfs $ \fp -> do
    SS.shelly $ SS.run_ "rm" [T.pack $ tpath <> "/" <> fp]
  dir <- getCurrentDirectory
  let tpath = dir <> "/.test"
  _ <- SS.shelly $ do
    SS.setenv "ERL_LIBS" (T.pack tpath)
    SS.run  "erl" ["-pa",T.pack (tpath), "-noshell","-eval" ,"('Test':main())()","-s","init","stop" ]
  return ()
  exitSuccess

runProject :: Opts.Parser (IO ())
runProject  =pure $ do
  dir <- getCurrentDirectory
  let tpath = dir <> "/ebin"
  isExist <- doesDirectoryExist hamlerlib
  _ <- SS.shelly $ do
    if isExist
      then SS.setenv "ERL_LIBS" (T.pack hamlerFile)
      else SS.setenv "ERL_LIBS" (T.pack $ dir <> ".deps/hamler")
    SS.run  "erl" ["-pa",T.pack (tpath), "-noshell","-eval" ,"('Main':main())()","-s","init","stop" ]
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
        , "main :: IO ()\n"
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
liblink :: T.Text
liblink = "https://github.com/hamler-lang/hamler.git"

hamlerlib :: String
hamlerlib =  $hamlerEnv <> "/lib"

hamlerFile :: String
hamlerFile = $hamlerEnv

initProject :: Opts.Parser (IO ())
initProject = pure $ do
  base <- getCurrentDirectory
  let dictlist' = fmap (\x -> base <> "/" <> x) dictlist
  mapM_  createDirectory dictlist'
  putStrLn "Generating src/Main.hm..."
  writeFile "src/Main.hm" helloHamler
  putStrLn "Generating Makefile..."
  writeFile "Makefile" makeFile
  isExist <- doesDirectoryExist hamlerlib
  if isExist
    then do
       return ()
    else do
       SS.shelly $ SS.run_ "git" ["clone",liblink,".deps/hamler"]

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

type CoreFilePath = FilePath
type DireFilePath = FilePath

myt :: ([CoreFilePath],[DireFilePath]) -> FilePath -> IO ([CoreFilePath],[DireFilePath])
myt (a,b) fp = do
  res <- doesDirectoryExist fp
  if res
    then return (a ,fp:b)
    else do
    if (take 4 $ reverse fp) == "lre."
      then return (fp:a,b)
      else return (a,b)

recErlc :: FilePath -> IO ()
recErlc fp = do
  ls <- listDirectory fp
  (coreFiles,dires) <- foldM myt ([],[]) $ fmap (\t -> fp ++ "/" ++ t) ls
  SS.shelly $ SS.command_ "erlc" ["-o" ,T.pack fp,"+to_core" ] (fmap T.pack coreFiles)
  mapConcurrently_ recErlc dires
