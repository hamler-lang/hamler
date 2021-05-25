{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Hamler.Erlang.PrettySimpleType where

import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Language.Hamler.Erlang.SimpleType
import Text.DocLayout
import Utils
import Prelude

instance Pretty Expr where
  pretty = cata go
    where
      go (ELCharF c) = char c
      go (ELIntegerF i) = text (show i)
      go (ELDoubleF i) = text (show i)
      go (ELAtomF i) = "" <> text i
      go (ELStringF i) = text i
      go EBinaryF = "EBinary"
      go (ETupleF ls) = "{" <> mSepD ls <> "}"
      go (EMapAF ls) = "#{" <> mSepD (map (\(a, b) -> a <> " => " <> b) ls) <> "}"
      go (EMapBF ls) = "#{" <> mSepD (map (\(a, b) -> a <> " := " <> b) ls) <> "}"
      go (EListF ls) = "[" <> mSepD ls <> "]"
      go (EPListF ls e) = "[" <> mSepD ls <> "|" <> e <> "]"
      go (EVarF s) = "V" <> text s
      go (ELambdaF ls e) = "fun(" <> mSepD ls <> ") -> " <> e
      go (EAppF e ls) = e <> "(" <> mSepD ls <> ")"
      go (EModuleCallF a b ls) = a <> ":" <> b <> "(" <> mSepD ls <> ")"
      go (ECaseF ls ecs) =
        "case " <> ls <> " of " <> cr
          <> nest 4 (mSepDCr $ map (\(bs, r) -> pretty bs <> " -> " <> r) ecs)
      go (EEqualExprF a b) = a <> " = " <> b
      go (ReceiveF ecs) = " reveive " <> cr <> nest 4 (mSepDCr $ map (\(bs, r) -> pretty bs <> " -> " <> r) ecs)
      go (ReceiveAfterF ecs (e1, e2)) = " reveive " <> cr <> nest 4 (mSepDCr $ map (\(bs, r) -> pretty bs <> " -> " <> r) ecs) <> cr <> " after " <> e1 <> " -> " <> e2
      go (ELetF ls e) = " let " <> cr <> nest 4 (mSepDCr $ map (\(a, b) -> a <> " = " <> b <> cr) ls) <> " in " <> e

removeMoreLambda :: Expr -> Expr
removeMoreLambda (ELambda ls e) = ELambda ls (removeMoreLambda' e) -- keep outside lambda
removeMoreLambda e = removeMoreLambda' e

removeMoreLambda' :: Expr -> Expr
removeMoreLambda' = cata go
  where
    go :: ExprF Expr -> Expr
    -- go (ELambdaF [] (EApp e ls')) = EApp e ls' -- \() -> f()   ---> f ()
    -- go (ELambdaF a (EApp e ls')) | a == ls' = e              -- error \(x,y) -> f (x,y) ---> f
    -- go (EAppF (ELambda a (EApp e ls')) b) | a == ls' = EApp e b -- right (\(x,y) -> f(x,y)) (1,2)  ---> f (1,2)
    go e = embed e

makeTopLetLam (ELet es (ELambda ls e)) = ELambda ls (ELet es e) -- change top let expr to lambda
makeTopLetLam (ELet es e) = ELambda [] (ELet es e) -- change top let expr to lambda
makeTopLetLam e = e

expandRec :: Int -> Map String Int -> Expr -> Expr
expandRec len m e = handle e
  where
    -- expandRec len m e = e

    handle (ELambda ls e) = ELambda (map (\i -> EVar $ "C_" ++ show i) [1 .. len] ++ ls) (cata go e)
    handle o = error (show o)

    go (EAppF (EVar s) ls) | M.member s m =
      case M.lookup s m of
        Nothing -> error (show e)
        Just i -> EApp (EVar $ "C_" ++ show i) (map (\i -> EVar $ "C_" ++ show i) [1 .. len] ++ ls)
    go e = embed e

addArgs :: Map String [String] -> Expr -> Expr
addArgs m e = cata go e
  where
    go (EAppF (EVar s) ls) =
      case M.lookup s m of
        Nothing -> EApp (EVar s) ls
        Just ns -> EApp (EVar s) (map EVar ns ++ ls)
    go e = embed e

instance Pretty Binder where
  pretty = cata go
    where
      go (BLCharF c) = char c
      go (BLIntegerF i) = text $ show i
      go (BLDoubleF i) = text $ show i
      go (BLAtomF i) = text i
      go (BLStringF i) = text i
      go (BListF ls) = "[" <> mSepD ls <> "]"
      go (BTupleF ls) = "{" <> mSepD ls <> "}"
      go (BMapBF ls) = "#{" <> mSepD (map (\(a, b) -> a <> " := " <> b) ls) <> "}"
      go BBinaryF = "EBinary"
      go (BPListF ls e) = "[" <> mSepD ls <> "|" <> e <> "]"
      go (BVarF s) = "V" <> text s
      go (BEqualExprF a b) = a <> " = " <> b
      go BPatNullF = "_"

handleLetRec :: (Map String Int, Int) -> Expr -> Expr
handleLetRec r@(varMap, len) = handle
  where
    cl :: [Expr] -> Expr -> Expr
    cl [] e = e
    cl (x : xs) e = ELambda [x] (cl xs e)

    handle e =
      cl (map (\i -> EVar $ "RECVAR" ++ show i) [1 .. len]) (handleLetRec' r e)

handleLetRec' :: (Map String Int, Int) -> Expr -> Expr
handleLetRec' (varMap, len) = cata go
  where
    ca :: [Expr] -> Expr -> Expr
    ca [] e = e
    ca (x : xs) e = ca xs $ EApp e [x]

    go (EVarF s) =
      case M.lookup s varMap of
        Nothing -> EVar s
        Just i -> ca ( map (\i -> EVar $ "RECVAR" ++ show i) [1 .. len]) (EVar $ "RECVAR" ++ show i)
    go e = embed e

handleLetRecApp :: Map String [String] -> Expr -> Expr
handleLetRecApp varMap = cata go
  where
    go (EVarF s) =
      case M.lookup s varMap of
        Nothing -> EVar s
        Just names ->
          let ca :: [Expr] -> Expr -> Expr
              ca [] e = e
              ca (x : xs) e = ca xs $ EApp e [x]

           in ca (map EVar names) (EVar s)
    go e = embed e
