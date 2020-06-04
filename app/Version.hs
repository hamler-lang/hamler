{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Paths_hamler as Paths

versionString :: String
versionString = showVersion Paths.version
