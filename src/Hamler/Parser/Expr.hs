{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hamler.Parser.Expr
-- Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
-- License     :  Apache Version 2.0
--
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
--------------------------------------------------------------------------------

module Hamler.Parser.Expr
  ( ifThenElse
  , caseOf
  , letBind
  , whereBind
  , guards
  , patMatch
  ) where



