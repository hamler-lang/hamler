-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Hamler
-- Copyright   :  (c) Feng Lee 2020-2021
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Feng Lee, feng@emqx.io
--                Yang M, yangm@emqx.io
-- Stability   :  experimental
-- Portability :  portable
--
-- The Hamler Programming Language.
--
-----------------------------------------------------------------------------
module Language.Hamler
  ( hello
  , version
  ) where

import Prelude
import Data.Version (Version)

import qualified Paths_hamler as Paths

hello :: IO ()
hello = putStrLn "Hello, Hamler!"

version :: Version
version = Paths.version

