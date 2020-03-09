{-# LANGUAGE DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.CodeGen.GenErl
-- Copyright   :  (c) Feng Lee 2020
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  stable
-- Portability :  portable
--
-- Generate erlang code from AST
--
-----------------------------------------------------------------------------

module Hamler.CodeGen.GenErl (GenErl, genErl) where

import Hamler.AST.SrcLoc
import Hamler.AST.Syntax

import Data.Char(toUpper)

class Annotated ast => GenErl ast where
  genErl :: ast SrcSpanInfo -> IO ()

instance GenErl Module where
  genErl (Module _ mmh oss ids decls) = do
    maybeGen mmh
    putStr "\n"
    genCont ids
    putStr "\n"
    genSepCont "\n" decls

instance GenErl ModuleHead where
  genErl (ModuleHead _ mn mwt mess) = do
    genErl mn
    maybeGen mess
    return ()

instance GenErl ModuleName where
  genErl (ModuleName _ n) = putStrLn $ "-module('" ++ n ++ "').\n"

instance GenErl ExportSpecList where
  genErl (ExportSpecList _ ess) = genCont ess

instance GenErl ExportSpec where
  genErl espec = case espec of
     EVar _ qn -> do
       putStr "-export(["
       genErl qn
       putStr "/1]).\n"
     _ -> return ()

instance GenErl ImportDecl where
  genErl (ImportDecl _ mn qf mpkg mas mispecs) = do
    putStr "-import("
    let (ModuleName _ n) = mn
       in putStr $ "'" ++ n ++ "'"
    putStr ", ["
    maybeGen mispecs
    putStr "]).\n"

instance GenErl ImportSpecList where
  genErl (ImportSpecList _l _hid ispecs) = genCont ispecs

instance GenErl ImportSpec where
  genErl ispec = case ispec of
    IVar _ qn  -> genErl qn >> putStr "/1"
    _ -> return ()

instance GenErl Decl where
  genErl decl = case decl of
    FunBind _ ms -> genCont ms

instance GenErl Match where
  genErl (Match _ n ps rhs _mbinds) = do
    genErl n
    putStr "("
    genSepCont "," ps
    putStr ") -> "
    genErl rhs
    putStr ".\n"

instance GenErl Pat where
  genErl pat = case pat of
    PVar _ n -> genErl $ upname n
    PLit _ _ lit -> genErl lit

instance GenErl Rhs where
  genErl (UnGuardedRhs _ exp) = genErl exp

instance GenErl Exp where
  genErl exp = case exp of
    Var _ qn -> genErl $ upqname qn
    Lit _ lit -> genErl lit
    InfixApp _ exp1 qop exp2 -> do
      genErl exp1
      putStr " "
      genErl qop
      putStr " "
      genErl exp2

instance GenErl QOp where
  genErl op = case op of
    QVarOp _ qn -> genErl qn
    QConOp _ qn -> genErl qn

instance GenErl QName where
  genErl qn = case qn of
    Qual _ _ n -> genErl n
    UnQual _ n -> genErl n
    _ -> return ()

instance GenErl Name where
  genErl n = case n of
    Ident _ str  -> putStr str
    Symbol _ str -> putStr str

instance GenErl Literal where
  genErl lit = case lit of
    Char       _ _ rw -> putStr ('$':rw)
    String     _ _ rw -> putStr ('\"':rw ++ "\"")
    Int        _ _ rw -> putStr rw
    Frac       _ _ rw -> putStr rw

genCont :: GenErl ast => [ast SrcSpanInfo] -> IO ()
genCont [] = return ()
genCont (a:asts) = genErl a >> genCont asts

genSepCont :: GenErl ast => String -> [ast SrcSpanInfo] -> IO ()
genSepCont _ [] = return ()
genSepCont _ ([x]) = genErl x
genSepCont sep (x:xs) = do
  genErl x
  putStr sep
  genCont xs

maybeGen :: GenErl ast => Maybe (ast SrcSpanInfo) -> IO ()
maybeGen (Just ast) = genErl ast
maybeGen Nothing = return ()

upqname :: QName SrcSpanInfo -> QName SrcSpanInfo
upqname (Qual l m n) = Qual l m (upname n)
upqname (UnQual l n) = UnQual l (upname n)
upqname other = other

upname :: Name SrcSpanInfo -> Name SrcSpanInfo
upname (Ident l s) = Ident l (capitalized s)
upname other = other

capitalized :: String -> String
capitalized [] = []
capitalized (x:xs) = toUpper x : xs

