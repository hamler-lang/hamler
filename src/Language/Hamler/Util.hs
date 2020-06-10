-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Hamler.Util
-- Copyright   :  (c) Feng Lee 2020
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Feng Lee, feng@emqx.io
--                Yang M, yangm@emqx.io
-- Stability   :  experimental
-- Portability :  portable
--
-- The experimental Util module.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Language.Hamler.Util where

import           Language.CoreErlang                as E
import           Language.PureScript.PSString
import           Prelude
import           Data.List
import qualified Data.Map                           as M


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
mm1 :: FunName
mm1 = FunName (Atom "module_info" , 1)

mm0 :: FunName
mm0 = FunName (Atom "module_info" , 0)


ppsToAtomExprs :: PSString -> Exprs
ppsToAtomExprs pps = Expr $ Constr $ Lit $ LAtom $ Atom $ decodePPS pps

stringToAtomExprs :: String -> Exprs
stringToAtomExprs s = Expr $ Constr $ Lit $ LAtom $ Atom s

mapsAtom :: Exprs
mapsAtom = stringToAtomExprs "maps"

getAtom :: Exprs
getAtom = stringToAtomExprs "get"

putAtom :: Exprs
putAtom = stringToAtomExprs "put"

mapInsertList :: Ord k => [(k,v)] -> M.Map k v -> M.Map k v
mapInsertList xs m0 = foldl' (\m (k,v) -> M.insert k v m ) m0 xs

decodePPS :: PSString -> String
decodePPS = decodeStringWithReplacement

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

getJust :: Maybe a -> a
getJust (Just x) = x
getJust _ = error "there is error"

tcc :: Char -> Char
tcc '.' =' '
tcc x = x

cModCall :: Int -> String -> String -> E.Expr
cModCall 0 s1 s2 = ModCall (ce s1 ,ce s2) []
  where ce s = Expr $ Constr $ Lit $ LAtom $ Atom s

cModCall n s1 s2 =netLambda1 (fmap cv [0..n-1]) [] (ce s1) (ce s2)
  where ce s = Expr $ Constr $ Lit $ LAtom $ Atom s
        cv i = E.Var $ Constr $ "_" ++ show i


netLambda1 :: [Var] -> [Var]  -> E.Exprs -> E.Exprs ->E.Expr
netLambda1 [] [] p1 p2 = error "nice"
netLambda1 [x] s p1 p2= Lam [x] (Expr . Constr $ ModCall (p1 ,p2) (fmap (Expr . Constr . EVar) (reverse $ x:s)))
netLambda1 (x:xs) s p1 p2 = Lam [x] (Expr $ Constr $ netLambda1 xs  (x:s) p1 p2)





