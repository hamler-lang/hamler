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
import qualified Data.Text.IO as TIO
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

funDef = FunDef (Constr $ FunName (Atom "test1" , 2)) (Constr $ makeFunErlang 2 "+")
funDef1 = FunDef (Constr $ FunName (Atom "test2" , 2)) (Constr $ makeFunErlang 2 "-")
tModule = E.Module (Atom "Main") [ FunName (Atom "test1" , 2)
                                 , FunName (Atom "test2" , 2)
                                 ] [] [ funDef
                                      , funDef1
                                      ]

t= TIO.writeFile ("tests/data/Main.core") (pack $ E.prettyPrint tModule)

t1 = TIO.readFile ("tests/data/Main.core")

t3 = do
  con <- t1
  case E.parseModule $ unpack con of
    Left e -> print e
    Right m -> print m




moduleInfo0 :: String -> FunDef
moduleInfo0 s = FunDef (Constr $ FunName (Atom "module_info" , 0))
                       (Constr $ Lam []
                          (Expr $ Constr $ ModCall ( Expr $ Constr $ Lit $ LAtom $ Atom "erlang"
                                   , Expr $ Constr $ Lit $ LAtom $ Atom "get_module_info"
                                   ) [(Expr $ Constr $ Lit $ LAtom $ Atom s)]
                          )
                       )


moduleInfo1 :: String -> FunDef
moduleInfo1 s = FunDef (Constr $ FunName (Atom "module_info" , 1))
                       ( Constr $ Lam [E.Var $ Constr $ "_0"]
                          (Expr $ Constr $  ModCall ( Expr $ Constr $ Lit $ LAtom $ Atom "erlang"
                                   , Expr $ Constr $ Lit $ LAtom $ Atom "get_module_info"
                                   ) [ Expr $ Constr $ Lit $ LAtom $ Atom s
                                     , Expr $ Constr $ EVar $ E.Var $ Constr $ "_0"]
                          )
                       )

mm1 = FunName (Atom "module_info" , 1)
mm0 = FunName (Atom "module_info" , 0)


-- 'module_info'/0 =
--     fun () ->
--  call 'erlang':'get_module_info'
--      ('Ring')
-- 'module_info'/1 =
--     fun (_0) ->
--  call 'erlang':'get_module_info'
--      ('Ring', _0)
