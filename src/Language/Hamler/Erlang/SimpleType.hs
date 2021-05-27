{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Hamler.Erlang.SimpleType where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Text.DocLayout
import Utils
import Prelude

data Expr
  = ELChar Char
  | ELInteger Integer
  | ELDouble Double
  | ELAtom String
  | ELString String
  | EBinary
  | ETuple [Expr]
  | --  aa => 1 map create
    EMapA [(Expr, Expr)]
  | EMapB [(Expr, Expr)]
  | --  [1,2,3,4]
    EList [Expr]
  | --  [1,2,3 | a]
    EPList [Expr] Expr
  | EVar String
  | ELambda [Expr] Expr

  | EApp Expr [Expr]

  | EModuleCall Expr Expr [Expr]
  | ECase Expr [ECaseAlt]
  | --  erlang '='
    --
    -- ------1---------
    -- %%erlang local var defined
    --  test(A) ->
    --    T = fun(B) -> B end.
    --    A.
    EEqualExpr Expr Expr
  | Receive [ECaseAlt]
  | ReceiveAfter [ECaseAlt] (Expr, Expr)
  | ELet [(Expr, Expr)] Expr
  deriving (Show, Eq)

--             binders resultExpr
type ECaseAlt = (Binder, Expr)

data Binder
  = BLChar Char
  | BLInteger Integer
  | BLDouble Double
  | BLAtom String
  | BLString String
  | BList [Binder]
  | BTuple [Binder]
  | BMapB [(Binder, Binder)]
  | BBinary
  | BPList [Binder] Binder
  | BVar String
  | -- ------2---------
    -- %%pattern match PatternAlias
    -- myfun(T) ->
    --     case T of
    --         {foo, Bar } = Var -> start(Var, stuff) end.
    -- attention that the position of Var as left of '='
    BEqualExpr Binder Binder
  | BPatNull
  deriving (Show, Eq)

makeBaseFunctor ''Expr

makeBaseFunctor ''Binder
