{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Compile as Compile
import           Data.Foldable (fold)
import qualified Options.Applicative as Opts
import           System.Environment (getArgs)
import qualified System.IO as IO
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Version (versionString)

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    cmd <- Opts.handleParseResult . execParserPure opts =<< getArgs
    cmd
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "The hamler compiler based on purescript 0.13.6"
    footerInfo  = Opts.footerDoc (Just footer)

    footer =
      mconcat
        [ para $
            "For help using each individual command, run `hamler COMMAND --help`. " ++
            "For example, `hamler compile --help` displays options specific to the `compile` command."
        , Doc.hardline
        , Doc.hardline
        , Doc.text $ "hamler" ++ versionString
        ]

    para :: String -> Doc.Doc
    para = foldr (Doc.</>) Doc.empty . map Doc.text . words

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo Opts.ShowHelpText mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands =
      (Opts.subparser . fold)
        [ Opts.command "build"
            (Opts.info Compile.command
              (Opts.progDesc "Compile hamler source files"))

        ,  Opts.command "init"
            (Opts.info Compile.initProject
              (Opts.progDesc "init hamler project"))

        ,  Opts.command "run"
            (Opts.info Compile.runProject
              (Opts.progDesc "run hamler project"))

        ]

