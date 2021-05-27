{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Hamler.Erlang.CodeGen where

import Control.Algebra (type (:+:))
import Control.Arrow (first)
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Optics
import Control.Exception
import Control.Monad (forM)
import Control.Monad.Compat (foldM)
import qualified Data.Bifunctor
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (pack, unpack)
import Erlang.Pretty
import qualified Language.Hamler.Erlang.TranslateSimpleType as Erlang
import Erlang.Type (Forms)
import qualified Erlang.Type as Erlang
import Language.Hamler.Erlang.PrettySimpleType
import Language.Hamler.Erlang.SimpleType as E
import Language.Hamler.Erlang.TranslateSimpleType
import Language.PureScript (Constraint, Expr (UnaryMinus), Ident (GenIdent, Ident, UnusedIdent), ModuleName (ModuleName), ProperName (ProperName), Qualified (Qualified), moduleNameFromString, runModuleName, showQualified)
import Language.PureScript.CoreFn as C
import Language.PureScript.Names (ProperNameType (ConstructorName))
import Language.PureScript.PSString (PSString, decodeStringWithReplacement)
import Optics hiding (view)
import Text.DocLayout
import Utils
import Prelude

data Tenv = Tenv
  { moduleName :: ModuleName,
    otherModuelMap :: Map (Qualified Ident) Bool,
    thisModuleMap :: Map (Qualified Ident) Bool,
    ffiMap :: Map (Qualified Ident) Int
  }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''Tenv

createTenv :: Module Ann -> Map (Qualified Ident) Bool -> Map (Qualified Ident) Int -> Tenv
createTenv m other ffi =
  Tenv
    { moduleName = mn,
      otherModuelMap = other,
      thisModuleMap = this,
      ffiMap = ffi
    }
  where
    isLambda :: C.Expr Ann -> Bool
    isLambda Abs {} = True
    isLambda (Constructor _ _ _ ls) | not (null ls) = True
    isLambda _ = False

    mn = C.moduleName m

    topBind :: Bind Ann -> [(Qualified Ident, Bool)]
    topBind (NonRec _ i e) = [(Qualified (Just mn) i, isLambda e)]
    topBind (Rec ls) = map (\((_, i), e) -> (Qualified (Just mn) i, isLambda e)) ls

    this = M.fromList $ concatMap topBind (moduleDecls m)

runIdent :: Ident -> String
runIdent (Ident i) = unpack i
runIdent (GenIdent Nothing n) = error $ "$" <> show n
runIdent (GenIdent (Just name) n) = error $ "$" <> unpack name <> show n
runIdent UnusedIdent = "unused"

rPropNameConstr :: ProperName 'ConstructorName -> E.Expr
rPropNameConstr (ProperName t) = ELAtom (unpack t)

rIdentVar :: Ident -> E.Expr
rIdentVar = EVar . runIdent

rIdentAtom :: Ident -> E.Expr
rIdentAtom = ELAtom . runIdent

rExprPss :: Has (Reader Tenv) sig m => E.Expr -> (PSString, C.Expr Ann) -> m E.Expr
rExprPss e (s, e1) = do
  e1' <- rExpr e1
  return $ EModuleCall (ELAtom "maps") (ELAtom "put") [ELAtom (decodeStringWithReplacement s), e1', e]

rDecls :: Has (Reader Tenv) sig m => Bind Ann -> m [(Qualified Ident, E.Expr)]
rDecls (NonRec _ i e) = do
  mn <- view @Tenv #moduleName
  e' <- rExpr e
  this <- view @Tenv #thisModuleMap
  case (M.lookup (Qualified (Just mn) i) this, e') of
    (Just False, ELambda _ _) -> return [(Qualified (Just mn) i, ELambda [] e')]
    _ -> return [(Qualified (Just mn) i, e')]
-- return $ (Qualified (Just mn) i, e')
rDecls (Rec ls) = forM ls $ \((_, i), e) -> do
  mn <- view @Tenv #moduleName
  e' <- rExpr e

  this <- view @Tenv #thisModuleMap
  case (M.lookup (Qualified (Just mn) i) this, e') of
    (Just False, ELambda _ _) -> return (Qualified (Just mn) i, ELambda [] e')
    _ -> return (Qualified (Just mn) i, e')

-- return $ (Qualified (Just mn) i, e')

rModule :: Has (Reader Tenv) sig m => Module Ann -> m Forms
rModule m = do
  let mn = C.moduleName m
  eps <- rExport m
  decs <- concat <$> mapM rDecls (moduleDecls m)
  exportFFi <- rExportFFi m
  return $
    toFormsN
      (moduleNameToString mn)
      (map (first qiToString) eps)
      (map (first qiToString) (decs ++ concat exportFFi))

moduleNameToString :: ModuleName -> String
moduleNameToString mn = unpack (runModuleName mn)

qiToString :: Qualified Ident -> String
qiToString (Qualified (Just _) i) = runIdent i
qiToString e = error (show e)

rExport :: Has (Reader Tenv) sig m => Module Ann -> m [(Qualified Ident, Bool)]
rExport m = do
  let eps = moduleExports m
  this <- view @Tenv #thisModuleMap
  forM eps $ \i -> do
    let qi = Qualified (Just (C.moduleName m)) i
    case M.lookup qi this of
      Just v -> return (qi, v)
      Nothing -> do
        ffi <- view @Tenv #ffiMap
        case M.lookup qi ffi of
          Just 0 -> return (qi, False)
          Just _ -> return (qi, True)
          Nothing -> error (show qi ++ show ffi)

rExportFFi :: Has (Reader Tenv) sig m => Module Ann -> m [[(Qualified Ident, E.Expr)]]
rExportFFi m = do
  let eps = moduleExports m
  mn <- view @Tenv #moduleName
  this <- view @Tenv #thisModuleMap
  forM eps $ \i -> do
    let qi = Qualified (Just (C.moduleName m)) i
    case M.lookup qi this of
      Just _ -> return []
      Nothing -> do
        ffi <- view @Tenv #ffiMap
        case M.lookup qi ffi of
          Just 0 -> return [(qi, EModuleCall (ELAtom $ unpack (runModuleName mn) ++ "FFI") (rIdentAtom i) [])]
          Just ls ->
            let cl :: [E.Expr] -> [E.Expr] -> E.Expr -> E.Expr
                cl [] ys e = EModuleCall (ELAtom $ unpack (runModuleName mn) ++ "FFI") e ys
                cl (x : xs) ys e = ELambda [x] (cl xs ys e)
                args = map (EVar . show) [1 .. ls]
             in return [(qi, cl args args (rIdentAtom i))]
          Nothing -> error "never happened"

--- >>> toQi $ toString (Qualified (Just (ModuleName [ProperName "T"])) (Ident "help"), True)
-- (Qualified (Just  [ "T"]) (Ident "help"),True)
toString :: (Qualified Ident, Bool) -> String
toString (Qualified (Just mn) i, b) = moduleNameToString mn ++ "~" ++ runIdent i ++ "~" ++ show b
toString _ = error "never happend"

toStrings :: [(Qualified Ident, Bool)] -> String
toStrings ls = L.intercalate "\n" (map toString ls)

--- >>> toQi "T~help~True"
-- (Qualified (Just  [ "T"]) (Ident "help"),True)
toQi :: String -> (Qualified Ident, Bool)
toQi s = (Qualified (Just (moduleNameFromString (pack va))) (Ident (pack vb)), read vc)
  where
    bs [] = []
    bs vs =
      let (a, b) = L.break (== '~') vs
       in a : bs (drop 1 b)
    [va, vb, vc] = bs s

--- >>> toStrings $ toQis "T~help~True\nT~help~True"
-- "T~help~True\nT~help~True"
toQis :: String -> [(Qualified Ident, Bool)]
toQis ls = map toQi (lines ls)

rExpr :: Has (Reader Tenv) sig m => C.Expr Ann -> m E.Expr
rExpr (Literal _ li) = rLiteral li
rExpr (Constructor _ _ pc []) =
  return $ ELambda [] (ETuple [rPropNameConstr pc])
rExpr (Constructor _ _ pc xs) =
  let cl :: [E.Expr] -> [E.Expr] -> E.Expr
      cl [] ys = ETuple (rPropNameConstr pc : ys) -- EApp e ys
      cl (x : ks) ys = ELambda [x] (cl ks ys)
      vrs = map rIdentVar xs
   in return $ cl vrs vrs
rExpr (Accessor _ s e) = do
  e' <- rExpr e
  return $ EModuleCall (ELAtom "maps") (ELAtom "get") [ELAtom (decodeStringWithReplacement s), e']
rExpr (ObjectUpdate _ e xs) = do
  e' <- rExpr e
  foldM rExprPss e' xs
rExpr (Abs _ arg e) = do
  e' <- rExpr e
  return $ ELambda [rIdentVar arg] e'
rExpr (App _ (Var _ qi@(Qualified (Just mn) i)) b) = do
  mn' <- view @Tenv #moduleName
  b' <- rExpr b
  if mn == mn'
    then do
      this <- view @Tenv #thisModuleMap
      case M.lookup qi this of
        Just True -> return $ EApp (rIdentAtom i) [b']
        Just False -> return $ EApp (EApp (rIdentAtom i) []) [b']
        Nothing -> do
          ffi <- view @Tenv #ffiMap
          case M.lookup qi ffi of
            Just v -> case v of
              0 -> return $ EApp (EModuleCall (ELAtom $ unpack (runModuleName mn) ++ "FFI") (rIdentAtom i) []) [b']
              s ->
                let cl :: [E.Expr] -> [E.Expr] -> E.Expr -> E.Expr
                    cl [] ys e = EModuleCall (ELAtom $ unpack (runModuleName mn) ++ "FFI") e ys
                    cl (x : xs) ys e = ELambda [x] (cl xs ys e)
                    args = map (EVar . show) [1 .. s]
                 in return $ EApp (cl args args $ rIdentAtom i) [b']
            Nothing -> error (show qi ++ "  " ++ show ffi) -- "never happened"
    else do
      other <- view @Tenv #otherModuelMap
      case M.lookup qi other of
        Nothing -> error "never happend"
        Just True -> return $ EModuleCall (ELAtom $ unpack (runModuleName mn)) (rIdentAtom i) [b']
        Just False -> return $ EApp (EModuleCall (ELAtom $ unpack (runModuleName mn)) (rIdentAtom i) []) [b']
rExpr (App _ ((Var _ (Qualified Nothing i))) b) = do
  b' <- rExpr b
  return $ EApp (rIdentVar i) [b']
rExpr (App _ a b) = do
  a' <- rExpr a
  b' <- rExpr b
  return $ EApp a' [b']
rExpr (Var _ qi@(Qualified (Just mn) i)) = do
  mn' <- view @Tenv #moduleName
  if mn == mn'
    then do
      this <- view @Tenv #thisModuleMap
      case M.lookup qi this of
        Just True -> return $ ELambda [EVar "VAR"] (EApp (rIdentAtom i) [EVar "VAR"])
        Just False -> return (EApp (rIdentAtom i) [])
        Nothing -> do
          ffi <- view @Tenv #ffiMap
          case M.lookup qi ffi of
            Nothing -> error "never happened"
            Just v -> case v of
              0 -> return (EModuleCall (ELAtom $ unpack (runModuleName mn) ++ "FFI") (rIdentAtom i) [])
              s ->
                let cl :: [E.Expr] -> [E.Expr] -> E.Expr -> E.Expr
                    cl [] ys e = EModuleCall (ELAtom $ unpack (runModuleName mn) ++ "FFI") e ys
                    cl (x : xs) ys e = ELambda [x] (cl xs ys e)

                    args = map (EVar . show) [1 .. s]
                 in return $ cl args args (rIdentAtom i)
    else do
      if qi == Qualified (Just (ModuleName [ProperName "Prim"])) (Ident "undefined")
        then return $ ELambda [EVar "Prim_undef"] (EVar "Prim_undef")
        else do
          other <- view @Tenv #otherModuelMap
          case M.lookup qi other of
            Just True -> return $ ELambda [EVar "FFiVar"] (EModuleCall (ELAtom $ unpack (runModuleName mn)) (rIdentAtom i) [EVar "FFiVar"])
            Just False -> return (EModuleCall (ELAtom $ unpack (runModuleName mn)) (rIdentAtom i) [])
            Nothing -> error (show qi ++ "  " ++ show other) -- "never happend"
rExpr (Var _ (Qualified Nothing i)) = return $ rIdentVar i
rExpr (Case _ e caseAlts) = do
  e' <- mapM rExpr e
  cas <- mapM rCaseAlt $ expandCase e caseAlts
  return $ ECase (toTupleOrUnchange e') cas
rExpr (C.Receive _ Nothing ls) = E.Receive <$> mapM rCaseAlt ls
rExpr (C.Receive _ (Just (i, e)) ls) = do
  e' <- rExpr e
  ls' <- mapM rCaseAlt ls
  return $ ReceiveAfter ls' (ELInteger i, e')
rExpr (Let _ bs e) = snd <$> runState @(Map String [String]) M.empty (rLetBind bs e)
rExpr (List _ ls e) = EPList <$> mapM rExpr ls <*> rExpr e

rLetBind :: Has (Reader Tenv :+: State (Map String [String])) sig m => [Bind Ann] -> C.Expr Ann -> m E.Expr
rLetBind binds e = do
  decls <- concat <$> mapM rLetBind' binds
  e' <- rExpr e
  varMap <- get @(Map String [String])
  return $ ELet decls (handleLetRecApp varMap e')

rLetBind' :: Has (Reader Tenv :+: State (Map String [String])) sig m => Bind Ann -> m [(E.Expr, E.Expr)]
rLetBind' (NonRec _ i e) = do
  e' <- rExpr e
  varMap <- get @(Map String [String])
  return [(rIdentVar i, handleLetRecApp varMap e')]
rLetBind' (Rec ls) = do
  recls <- mapM (\((_, i), e) -> (runIdent i,) <$> rExpr e) ls
  let names = map fst recls
      varMap = M.fromList (zip names [1 ..])
      hand = handleLetRec (varMap, length recls)
  modify @(Map String [String]) (M.union (M.fromList $ zip names (repeat names)))
  return $ map (Data.Bifunctor.bimap EVar hand) recls

expandCase :: [C.Expr Ann] -> [CaseAlternative Ann] -> [CaseAlternative Ann]
expandCase _ [] = []
expandCase es (x@(CaseAlternative _ (Right _)) : xs) = x : expandCase es xs
expandCase es ((CaseAlternative bs (Left ls)) : xs) = CaseAlternative bs (Right (changeLeft (extractAnn (head es)) ls)) : expandCase es xs
  where
    changeLeft :: Ann -> [(Guard Ann, C.Expr Ann)] -> C.Expr Ann
    changeLeft ann [] = if null xs then C.Literal ann (BooleanLiteral False) else Case ann es (expandCase es xs)
    changeLeft ann ((gval, e) : ys) =
      Case
        ann
        [gval]
        [ CaseAlternative [C.LiteralBinder ann (BooleanLiteral True)] (Right e),
          CaseAlternative [C.LiteralBinder ann (BooleanLiteral False)] (Right $ changeLeft ann ys)
        ]

toTupleOrUnchange :: [E.Expr] -> E.Expr
toTupleOrUnchange [] = error "never happend"
toTupleOrUnchange [e] = e
toTupleOrUnchange ls = ETuple ls

toTupleOrUnchangeB :: [E.Binder] -> E.Binder
toTupleOrUnchangeB [] = error "never happend"
toTupleOrUnchangeB [e] = e
toTupleOrUnchangeB ls = BTuple ls

rCaseAlt :: Has (Reader Tenv) sig m => C.CaseAlternative Ann -> m E.ECaseAlt
rCaseAlt (CaseAlternative b (Right e)) = do
  e' <- rExpr e
  return (toTupleOrUnchangeB $ map rBinder b, e')
rCaseAlt c = error (show c)

rLiteral :: Has (Reader Tenv) sig m => Literal (C.Expr Ann) -> m E.Expr
rLiteral (BooleanLiteral True) = return $ ELAtom "true"
rLiteral (BooleanLiteral False) = return $ ELAtom "false"
rLiteral (NumericLiteral (Left i)) = return $ ELInteger i
rLiteral (NumericLiteral (Right i)) = return $ ELDouble i
rLiteral (CharLiteral c) = return $ ELChar c
rLiteral (StringLiteral s) = return $ ELString (decodeStringWithReplacement s)
rLiteral (ListLiteral xs) = EList <$> mapM rExpr xs
rLiteral (AtomLiteral s) = return $ ELAtom (decodeStringWithReplacement s)
rLiteral (ObjectLiteral xs) =
  EMapA <$> mapM (\(s, e) -> (ELAtom $ decodeStringWithReplacement s,) <$> rExpr e) xs
rLiteral (BinaryLiteral _) = return EBinary -- TODO
rLiteral (Tuple2Literal a b) = ETuple <$> mapM rExpr [a, b]
rLiteral (TupleLiteral xs) = ETuple <$> mapM rExpr xs

rIndentBVar :: Ident -> E.Binder
rIndentBVar = BVar . runIdent

rPropNameConstrB :: ProperName 'ConstructorName -> E.Binder
rPropNameConstrB (ProperName t) = BLAtom (unpack t)

--  erlang '='
--
-- ------1---------
-- %%erlang local var defined
--  test(A) ->
--    T = fun(B) -> B end.
--    A.
--
-- ------2---------
-- %%pattern match PatternAlias
-- myfun(T) ->
--     case T of
--         {foo, Bar } = Var -> start(Var, stuff) end.
-- attention that the position of Var as left of '='
rBinder :: C.Binder Ann -> E.Binder
rBinder (NullBinder _) = BPatNull
rBinder (LiteralBinder _ lb) = rLiteralBinder lb
rBinder (VarBinder _ i) = rIndentBVar i
rBinder (ConstructorBinder _ _ (Qualified _ p) ls) = BTuple (rPropNameConstrB p : map rBinder ls)
rBinder (NamedBinder _ a b) =
  BEqualExpr (rBinder b) (rIndentBVar a)
rBinder (MapBinder _ ls) = BMapB $ map (Data.Bifunctor.bimap rBinder rBinder) ls
rBinder (BinaryBinder _ _) = undefined -- TODO
rBinder (ListBinder _ bs b) = BPList (map rBinder bs) (rBinder b)

rLiteralBinder :: Literal (C.Binder Ann) -> E.Binder
rLiteralBinder (BooleanLiteral True) = BLAtom "true"
rLiteralBinder (BooleanLiteral False) = BLAtom "false"
rLiteralBinder (NumericLiteral (Left i)) = BLInteger i
rLiteralBinder (NumericLiteral (Right i)) = BLDouble i
rLiteralBinder (CharLiteral c) = BLChar c
rLiteralBinder (StringLiteral s) = BLString (decodeStringWithReplacement s)
rLiteralBinder (AtomLiteral s) = BLAtom (decodeStringWithReplacement s)
rLiteralBinder (ListLiteral xs) = BList (map rBinder xs)
rLiteralBinder (Tuple2Literal a b) = BTuple [rBinder a, rBinder b]
rLiteralBinder (TupleLiteral xs) = BTuple (map rBinder xs)
rLiteralBinder (ObjectLiteral xs) =
  BMapB (fmap (\(s, e) -> (BLAtom $ decodeStringWithReplacement s, rBinder e)) xs)
rLiteralBinder e = error (show e)
