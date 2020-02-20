{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hamler.Parser
-- Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
-- License     :  Apache Version 2.0
--
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implement a parser for hamler language.
--------------------------------------------------------------------------------

module Hamler.Parser
  ( parseStr
  , parseFile
  ) where

import Text.Megaparsec ( parse
                       , runParser
                       )
import qualified Hamler.Parser.Lexer as Lexer

import qualified Data.Text as T

-- parseStr :: Lexer.Parser -> T.Text -> undefined
parseStr parser = parse parser ""

parseFile parser file = runParser parser file <$> readFile file

