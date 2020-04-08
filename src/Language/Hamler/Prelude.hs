-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Hamler.Prelude
-- Copyright   :  (c) Feng Lee 2020
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Feng Lee, feng@emqx.io
--                Yang M, yangm@emqx.io
-- Stability   :  experimental
-- Portability :  portable
--
-- The experimental prelude module.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Hamler.Prelude where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List                          as LL
import           Data.Map                           as M
import           Data.Text                          (Text, unpack,pack)
import qualified Data.Text.Lazy                     as L
import           Debug.Trace
import           Language.CoreErlang                as E
import qualified Language.PureScript.AST.Literals   as L
import           Language.PureScript.CoreFn         as C
import           Language.PureScript.CoreFn.Binders
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Prelude

type VarQuantity = Int

makeFun :: VarQuantity -> String -> String -> E.Expr
makeFun args a b = Lam vars (Expr $ Constr $ ModCall ( Expr $ Constr $ Lit $ LAtom $ Atom a
                                                     , Expr $ Constr $ Lit $ LAtom $ Atom b
                                                     ) exprs)
  where vars = fmap (\i -> E.Var $ Constr $ "_" ++ show i) [0..args-1]
        exprs = fmap (\i -> Expr $ Constr $ EVar $ E.Var $ Constr $ "_" ++ show i) [0..args-1]

makeFunErlang :: VarQuantity -> String -> E.Expr
makeFunErlang  i s = makeFun i "erlang" s

createT :: String -> String -> Int -> (Text,(Int,E.Expr))
createT ffiName erlang i =
  ( showQualified runIdent $  Qualified (Just (ModuleName [ProperName "Prelude"])) (Ident (pack ffiName))
  , (i,makeFunErlang i erlang)
  )

plist =fmap (\(a,b,c) -> createT a b c )
       [ ("add","+",2)
       , ("sub","-",2)
       , ("greaterThan",">",2)
       , ("lessThan","<",2)
       ]

