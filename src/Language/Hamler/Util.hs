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
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


module Language.Hamler.Util where

import Data.List
import qualified Data.Map as M
import Data.String (IsString)
import Data.Text (pack)
import Language.CoreErlang as E
import Language.PureScript.PSString
import Prelude

ann :: (IsString a, Ann m a) => (a -> m a) -> m a
ann = annText ""

elist :: [Expr Text] -> Expr Text -> Expr Text
elist [] e = e
elist (x : xs) e = ann . EList $ LL x (elist xs e)

elist' :: [Expr Text] -> Expr Text
elist' [] = ann . ELit $ ann LNil
elist' (x : xs) = ann . EList $ LL x (elist' xs)

elistpat :: [Pat Text] -> Pat Text -> Pat Text
elistpat [] l = l
elistpat (x : xs) l = ann . PList $ LL x (elistpat xs l)

elistpat' :: [Pat Text] -> Pat Text
elistpat' [] = ann . PLiteral $ ann LNil
elistpat' (x : xs) = ann . PList $ LL x (elistpat' xs)

moduleInfo0 :: Text -> FunDef Text
moduleInfo0 s =
  FunDef
    (ann $ FunName (ann $ Atom "module_info") 0)
    ( ann $
        Fun
          []
          ( ann
              . Expr
              . ann
              $ EModCall
                (stringToAtomExprs "erlang")
                (stringToAtomExprs "get_module_info")
                [(stringToAtomExprs s)]
          )
    )

moduleInfo1 :: Text -> FunDef Text
moduleInfo1 s =
  FunDef
    (ann $ FunName (ann $ Atom "module_info") 1)
    ( ann $
        Fun
          [ann $ E.Var "_0"]
          ( ann
              . Expr
              . ann
              $ EModCall
                (stringToAtomExprs "erlang")
                (stringToAtomExprs "get_module_info")
                [ stringToAtomExprs s,
                  ann . Expr . ann . EVar . ann $ E.Var "_0"
                ]
          )
    )

mm1 :: FunName Text
mm1 = ann $ FunName (ann $ Atom "module_info") 1

mm0 :: FunName Text
mm0 = ann $ FunName (ann $ Atom "module_info") 0

decodePPS :: PSString -> String
decodePPS = decodeStringWithReplacement

ppsToAtomExprs :: PSString -> Exprs Text
ppsToAtomExprs pps = stringToAtomExprs . pack $ decodePPS pps

stringToAtomExprs :: Text -> Exprs Text
stringToAtomExprs s = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom s

mapsAtom :: Exprs Text
mapsAtom = stringToAtomExprs "maps"

getAtom :: Exprs Text
getAtom = stringToAtomExprs "get"

putAtom :: Exprs Text
putAtom = stringToAtomExprs "put"

mapInsertList :: Ord k => [(k, v)] -> M.Map k v -> M.Map k v
mapInsertList xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

getJust :: Maybe a -> a
getJust (Just x) = x
getJust _ = error "strange error"

tcc :: Char -> Char
tcc '.' = ' '
tcc x = x

cModCall :: Int -> Text -> Text -> E.Expr Text
cModCall 0 s1 s2 = ann $ EModCall (stringToAtomExprs s1) (stringToAtomExprs s2) []
cModCall n s1 s2 = netLambda1 (fmap cv [0 .. n -1]) [] (stringToAtomExprs s1) (stringToAtomExprs s2)
  where
    cv i = ann $ E.Var $ pack $ "_" ++ show i

netLambda1 :: [Var Text] -> [Var Text] -> E.Exprs Text -> E.Exprs Text -> E.Expr Text
netLambda1 [] [] _ _ = error "strange error "
netLambda1 [x] s p1 p2 =
  ann . EFun . ann $ Fun [x] (ann . Expr . ann $ EModCall p1 p2 (fmap (ann . Expr . ann . EVar) (reverse $ x : s)))
netLambda1 (x : xs) s p1 p2 =
  ann . EFun . ann $ Fun [x] (ann . Expr $ netLambda1 xs (x : s) p1 p2)
netLambda1 _ _ _ _ = error "stringe error"

aPrimop :: [Exprs Text] -> Exprs Text
aPrimop v =
  ann . Expr . ann
    . EPrimOp
      (Atom "match_fail" "")
    $ [ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "function_clause"] ++ v
