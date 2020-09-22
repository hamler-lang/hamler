{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Version
-- Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Feng Lee, feng@emqx.io
--                Yang M, yangm@emqx.io
-- Stability   :  experimental
-- Portability :  portable
--
-- Version...
--
-----------------------------------------------------------------------------
module Version where

import Data.Version (showVersion)
import Prelude
import Paths_hamler as Paths
import Language.Haskell.TH.Syntax (Exp, Q, runIO)
import System.Environment (lookupEnv)
import Data.FileEmbed (strToExp)



versionString :: String
versionString = showVersion Paths.version

hamlerEnv :: Q Exp
hamlerEnv = do
  r <- runIO $ lookupEnv "HAMLER_HOME"
  case r of
    Nothing -> error "There is no HAMLER_HOME env var "
    Just x -> strToExp x

