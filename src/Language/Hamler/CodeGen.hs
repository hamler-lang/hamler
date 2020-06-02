{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Hamler.CodeGen
-- Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Feng Lee, feng@emqx.io
--                Yang M, yangm@emqx.io
-- Stability   :  experimental
-- Portability :  portable
--
-- Generate CoreErlang AST from Purescript source code.
-----------------------------------------------------------------------------

module Language.Hamler.CodeGen
  ( moduleToErl
  , runTranslate
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.List as LL
import Data.Map as M
import Data.Text (Text, toLower, unpack)
import qualified Data.Text.Lazy as L
import Language.CoreErlang as E
import Language.Hamler.Util
import qualified Language.PureScript.AST.Literals as L
import Language.PureScript.CoreFn as C
import Language.PureScript.Names
import Text.Pretty.Simple
import Prelude

type MError = String

type MLog = String

type Translate = ExceptT MError (StateT GState (Writer MLog))

data GState
  = GState
      { _gsmoduleName :: ModuleName,
        _ffiFun :: M.Map Text (Int, E.Expr),
        _globalVar :: M.Map Text Int,
        _localVar :: M.Map Text Int,
        _letMap :: M.Map Text (Int, E.Expr),
        _binderVarIndex :: Int,
        _modInfoMap :: M.Map Text (M.Map Text Int),
        _isInline :: Bool
      }
  deriving (Show)

makeLenses ''GState

emptyGState :: GState
emptyGState = GState (ModuleName []) M.empty M.empty M.empty M.empty 0 M.empty False

runTranslate ::
  Bool ->
  (M.Map Text (M.Map Text Int)) ->
  [(Text, (Int, E.Expr))] ->
  Translate a ->
  (((Either MError a), GState), MLog)
runTranslate isinline modinfos plist translate =
  runWriter $
    runStateT
      (runExceptT translate)
      ( (emptyGState & ffiFun .~ (M.fromList plist))
          & modInfoMap .~ modinfos
          & isInline .~ isinline
      )

-- | CoreFn Module to CoreErlang AST
moduleToErl :: C.Module C.Ann -> Translate E.Module
moduleToErl C.Module {..} = do
  modify (\x -> x & gsmoduleName .~ moduleName)
  let tt = unpack $ runModuleName moduleName
  funDecls <- concat <$> mapM bindToErl moduleDecls
  let funDecls' = moduleInfo0 tt : moduleInfo1 tt : funDecls
  gs <- get
  exports <- forM moduleExports $ \ident -> do
    let name = showQualified runIdent $ mkQualified ident (gs ^. gsmoduleName)
        wname = runIdent ident
    case M.lookup name (gs ^. globalVar) of
      Just args -> return $ (Nothing, FunName (Atom $ unpack wname, toInteger args))
      Nothing -> case M.lookup name (gs ^. ffiFun) of
        Just (args, e) ->
          return $
            ( Just
                ( FunDef
                    (Constr $ FunName (Atom $ unpack wname, toInteger args))
                    (Constr $ e)
                ),
              FunName (Atom $ unpack wname, toInteger args)
            )
        Nothing -> throwError $ (unpack $ runModuleName $ gs ^. gsmoduleName) <> ":The function [" <> unpack wname <> "] is not implemented!"
  return $
    E.Module
      (Atom $ unpack $ runModuleName moduleName)
      (mm1 : mm0 : (fmap snd exports))
      []
      ( if gs ^. isInline
          then (funDecls' <> (fmap (getJust . fst) $ Prelude.filter (isJust . fst) exports))
          else
            ( funDecls'
                <> ( fmap
                       ( \(name, (args, expr)) ->
                           FunDef
                             (Constr $ FunName (Atom $ last $ words $ fmap tcc $ unpack name, toInteger args))
                             (Constr expr)
                       )
                       (M.toList $ gs ^. ffiFun)
                   )
            )
      )

-- | CoreFn Bind to CoreErlang FunDef
bindToErl :: C.Bind C.Ann -> Translate [FunDef]
bindToErl (NonRec _ ident e) = do
  modify (\x -> x & binderVarIndex .~ 100)
  e' <- exprToErl e
  gs <- get
  let name = showQualified runIdent $ mkQualified ident (gs ^. gsmoduleName)
      wname = runIdent ident
  case e' of
    Lam vrs _ -> do
      modify (\x -> x & globalVar %~ M.insert name (length vrs))
      return $ [FunDef (Constr $ FunName (Atom $ unpack wname, toInteger $ length vrs)) (Constr e')]
    _ -> do
      modify (\x -> x & globalVar %~ M.insert name 0)
      return $
        [ FunDef
            (Constr $ FunName (Atom $ unpack wname, 0))
            (Constr $ Lam [] (Expr $ Constr $ e'))
        ]
bindToErl (C.Rec xs) = do
  modify (\x -> x & binderVarIndex .~ 100)
  gs <- get
  let ns = fmap (\((_, a), b) -> (mkname gs a, getArgNum b)) xs
  modify (\x -> x & globalVar %~ mapInsertList ns)
  concat <$> mapM (\((_, a), b) -> bindToErl (NonRec undefined a b)) xs
  where
    expend s (Abs _ ident expr) = expend (ident : s) expr
    expend s exp1 = (s, exp1)
    getArgNum s = length $ fst $ expend [] s
    mkname gs ident = showQualified runIdent $ mkQualified ident (gs ^. gsmoduleName)

bindToLetFunDef :: C.Bind C.Ann -> Translate ()
bindToLetFunDef (NonRec _ ident e) = do
  e' <- exprToErl e
  let name = runIdent ident
  case e' of
    Lam vrs _ -> do
      modify (\x -> x & letMap %~ M.insert name (length vrs, e'))
    _ -> do
      modify (\x -> x & letMap %~ M.insert name (0, Lam [] (Expr $ Constr $ e')))
bindToLetFunDef x = error $ L.unpack $ pShow x

-- | CoreFn Expr to CoreErlang Expr
exprToErl :: C.Expr C.Ann -> Translate E.Expr
exprToErl (Literal _ l) = literalToErl l
exprToErl (Constructor _ _ (ProperName p) ids) = do
  let args = length ids
      popName = unpack p
      popAtom = Expr $ Constr $ Lit $ LAtom $ Atom $ popName
      cvar i = E.Var $ Constr ("_" <> show i)
      vars = fmap cvar [0 .. args -1]
      ckv i = Expr $ Constr $ EVar $ cvar i
      cm = Tuple $ popAtom : fmap ckv [0 .. args -1]
  return $ Lam vars (Expr $ Constr $ cm)
exprToErl (Accessor _ pps e) = do
  e' <- exprToErl e
  return $ ModCall (mapsAtom, getAtom) [ppsToAtomExprs pps, Expr $ Constr e']
exprToErl (ObjectUpdate _ e xs) = do
  e' <- exprToErl e
  let foldFun m (k, v) = do
        v' <- exprToErl v
        return $
          ModCall
            (mapsAtom, putAtom)
            [ppsToAtomExprs k, Expr $ Constr v', Expr $ Constr m]
  foldM foldFun e' xs
exprToErl t@(Abs _ _ _) = do
  gs <- get
  let (xs, e') = expend [] t
      xs' = runIdent <$> reverse xs
      allVar = M.size (gs ^. localVar)
      iks = zip xs' [allVar ..]
      vars = fmap (\(_, i0) -> E.Var $ Constr ("_" <> show i0)) iks
  modify (\x -> x & localVar %~ mapInsertList iks)
  e'' <- exprToErl e'
  case e'' of
    Lam cv ce -> do
      put gs
      return $ Lam (vars <> cv) ce
    _ -> do
      put gs
      return $ Lam vars (Expr $ Constr e'')
  where
    expend s (Abs _ ident expr) = expend (ident : s) expr
    expend s exp1 = (s, exp1)
exprToErl t@(C.App _ _ _) = do
  let (e', xs) = expend t []
  e'' <- exprToErl e'
  xs' <- mapM exprToErl xs
  case e'' of
    (E.Fun (E.FunName (Atom _, args))) -> do
      case args == (fromIntegral $ length xs') of
        True -> return $ E.App (Expr $ Constr e'') (fmap (Expr . Constr) xs')
        False ->
          if (fromIntegral $ length xs') > args
            then do
              let (xs1, xs2) = Prelude.splitAt (fromIntegral args) xs'
              return $
                E.App
                  (Expr $ Constr $ E.App (Expr $ Constr e'') $ fmap (Expr . Constr) xs1)
                  (fmap (Expr . Constr) xs2)
            else do
              gs <- get
              let detal = args - (fromIntegral $ length xs')
                  allVar = M.size $ gs ^. localVar
                  vars = fmap (\i -> E.Var $ Constr ("_" <> show i)) [allVar .. allVar + fromIntegral detal -1]
              return $ Lam vars $ Expr $ Constr $
                E.App
                  (Expr $ Constr e'')
                  ( (fmap (Expr . Constr) xs')
                      <> (fmap (Expr . Constr . EVar) vars)
                  )
    m@(E.ModCall ((Expr (Constr (Lit (LAtom (Atom s1))))), s2) mcxs) -> do
      gs <- get
      let mn = unpack $ runModuleName $ gs ^. gsmoduleName
      if s1 == mn
        then return m
        else do
          let args = length mcxs
          if args == (length xs')
            then return $ E.ModCall (Expr $ Constr $ Lit $ LAtom $ Atom s1, s2) $ fmap (Expr . Constr) xs'
            else
              if (length xs' > args)
                then do
                  let (xs1, xs2) = Prelude.splitAt (fromIntegral args) xs'
                      ttt = E.ModCall (Expr $ Constr $ Lit $ LAtom $ Atom s1, s2) $ fmap (Expr . Constr) xs1
                  return $ E.App (Expr $ Constr $ ttt) (fmap (Expr . Constr) xs2)
                else do
                  let detal = args - (length xs')
                      allVar = M.size $ gs ^. localVar
                      vars = fmap (\i -> E.Var $ Constr ("_" <> show i)) [allVar .. allVar + fromIntegral detal -1]
                      ttt t1 = E.Expr $ Constr $ E.ModCall (Expr $ Constr $ Lit $ LAtom $ Atom s1, s2) $ t1
                  return $ Lam vars $ ttt (fmap (Expr . Constr) xs' <> fmap (Expr . Constr . EVar) vars)
    _ -> return $ E.App (Expr $ Constr e'') $ fmap (Expr . Constr) xs'
  where
    expend (C.App _ l r) s = expend l (r : s)
    expend e s = (e, s)
exprToErl (C.Var _ qi@(Qualified _ tema)) = do
  let name = showQualified runIdent qi
      wname = runIdent tema
  gs <- get
  case M.lookup name (gs ^. localVar) of
    Just i -> return $ E.EVar $ E.Var $ Constr $ "_" <> show i
    Nothing -> case M.lookup name (gs ^. letMap) of
      Just (0, e) -> return $ E.App (Expr $ Constr e) []
      Just (_, e) -> return e
      Nothing -> case M.lookup name (gs ^. globalVar) of
        Just a -> case a of
          0 -> return $ E.App (Expr $ Constr $ E.Fun $ E.FunName (Atom (unpack wname), 0)) []
          x -> return $ E.Fun $ E.FunName (Atom (unpack wname), toInteger x)
        Nothing -> case M.lookup name (gs ^. ffiFun) of
          Just (0, e) -> return $ E.App (Expr $ Constr e) []
          Just (x, e) ->
            return $
              if gs ^. isInline
                then e
                else E.Fun $ E.FunName (Atom (unpack wname), toInteger x)
          Nothing -> case qi of
            Qualified (Just mn) _ -> do
              let mn' = runModuleName mn
                  funName = showQualified runIdent qi
                  res = do
                    r1 <- M.lookup mn' (gs ^. modInfoMap)
                    M.lookup funName r1
              case res of
                Nothing ->
                  throwError $ unpack (runModuleName $ gs ^. gsmoduleName) <> " --> There is no such function " <> unpack mn' <> ":" <> unpack wname
                Just i -> return $ cModCall i (unpack mn') (unpack wname)
            Qualified Nothing _ ->
              throwError $ "Did not find this variable: " <> show qi <> "<-->" <> show gs
exprToErl (C.Let _ bs e) = do
  mapM_ bindToLetFunDef bs
  e' <- exprToErl e
  return e'
exprToErl (C.Case _ es alts) = do
  es' <- mapM exprToErl es
  alts' <- mapM altToErl alts
  return $ E.Case (E.Exprs $ Constr $ fmap Constr es') alts'

-- | CoreFn Alt to CoreErlang Alt
altToErl :: CaseAlternative C.Ann -> Translate (E.Ann E.Alt)
altToErl (CaseAlternative bs res) = do
  pats <- mapM binderToPat bs
  let guard1 = Guard $ Expr $ Constr $ Lit $ LAtom $ Atom "true"
  case res of
    Right expr -> do
      expr' <- exprToErl expr
      return $ Constr $
        E.Alt
          (Pats $ Constr $ fmap Constr $ pats)
          guard1
          (Expr $ Constr $ expr')
    Left xs -> do
      xs' <- guardToErl xs
      return $ Constr $
        E.Alt
          (Pats $ Constr $ fmap Constr $ pats)
          guard1
          (Exprs $ Constr [xs'])

guardToErl :: [(C.Guard C.Ann, C.Expr C.Ann)] -> Translate (E.Ann E.Expr)
guardToErl [] = do
  return $ Constr $ Lit $ LNil
guardToErl ((g, e) : xs) = do
  g' <- exprToErl g
  e' <- exprToErl e
  xs' <- guardToErl xs
  let true = Pat $ Constr $ PLit $ LAtom $ Atom "true"
      false = Pat $ Constr $ PLit $ LAtom $ Atom "false"
      guard1 = Guard $ Expr $ Constr $ Lit $ LAtom $ Atom "true"
      altTrue = E.Alt true guard1 (Expr $ Constr e')
      altFalse = E.Alt false guard1 $ Expr xs'
  return $ Constr $ E.Case (E.Expr $ Constr $ g') [Constr altTrue, Constr altFalse]

-- | CoreFn Literal to CoreErlang Expr
literalToErl :: L.Literal (C.Expr C.Ann) -> Translate E.Expr
literalToErl (NumericLiteral (Left i)) = return $ Lit $ LInt i
literalToErl (NumericLiteral (Right i)) = return $ Lit $ LFloat i
literalToErl (StringLiteral s) = return $ Lit $ LString $ decodePPS s
literalToErl (AtomLiteral s) = return $ Lit $ LAtom $ Atom $ decodePPS s
literalToErl (CharLiteral c) = return $ Lit $ LChar c
literalToErl (BooleanLiteral True) = return $ Lit $ LAtom (Atom "true")
literalToErl (BooleanLiteral False) = return $ Lit $ LAtom (Atom "false")
literalToErl (ListLiteral xs) = do
  xs' <- mapM exprToErl xs
  return $ E.List (L $ fmap (Expr . Constr) xs')
literalToErl (TupleLiteral a b) = do
  a' <- exprToErl a
  b' <- exprToErl b
  return $ Tuple [Expr $ Constr a', Expr $ Constr $ b']
literalToErl (TupleLiteral3 a b c) = do
  a' <- exprToErl a
  b' <- exprToErl b
  c' <- exprToErl c
  return $ Tuple [Expr $ Constr a', Expr $ Constr $ b', Expr $ Constr $ c']
literalToErl (TupleLiteral4 a b c d) = do
  a' <- exprToErl a
  b' <- exprToErl b
  c' <- exprToErl c
  d' <- exprToErl d
  return $ Tuple [Expr $ Constr a', Expr $ Constr $ b', Expr $ Constr $ c', Expr $ Constr $ d']
literalToErl (TupleLiteral5 a b c d e) = do
  a' <- exprToErl a
  b' <- exprToErl b
  c' <- exprToErl c
  d' <- exprToErl d
  e' <- exprToErl e
  return $ Tuple [Expr $ Constr a', Expr $ Constr $ b', Expr $ Constr $ c', Expr $ Constr $ d', Expr $ Constr $ e']
literalToErl (TupleLiteral6 a b c d e f) = do
  a' <- exprToErl a
  b' <- exprToErl b
  c' <- exprToErl c
  d' <- exprToErl d
  e' <- exprToErl e
  f' <- exprToErl f
  return $ Tuple [Expr $ Constr a', Expr $ Constr $ b', Expr $ Constr $ c', Expr $ Constr $ d', Expr $ Constr $ e', Expr $ Constr $ f']
literalToErl (TupleLiteral7 a b c d e f g) = do
  a' <- exprToErl a
  b' <- exprToErl b
  c' <- exprToErl c
  d' <- exprToErl d
  e' <- exprToErl e
  f' <- exprToErl f
  g' <- exprToErl g
  return $ Tuple [Expr $ Constr a', Expr $ Constr $ b', Expr $ Constr $ c', Expr $ Constr $ d', Expr $ Constr $ e', Expr $ Constr $ f', Expr $ Constr $ f']
literalToErl (ObjectLiteral xs) = do
  xs' <- forM xs $ \(pps, e) -> do
    e' <- exprToErl e
    return
      ( ppsToAtomExprs pps,
        Expr $ Constr e'
      )
  return $ EMap $ E.Map xs'
literalToErl (BinaryLiteral xs) = return $ Binary $ fmap tupleToBinaryVal xs

-- | Binder to Pat
binderToPat :: Binder C.Ann -> Translate Pat
binderToPat (NullBinder _) = do
  gs <- get
  let i = gs ^. binderVarIndex
  modify (\x -> x & binderVarIndex %~ (+ 1))
  return $ PVar $ E.Var $ Constr $ "_" <> show i
binderToPat (LiteralBinder _ l) = literalBinderToPat l
binderToPat (VarBinder _ i) = do
  gs <- get
  let index1 = gs ^. binderVarIndex
  modify (\x -> x & localVar %~ M.insert (runIdent i) index1)
  modify (\x -> x & binderVarIndex %~ (+ 1))
  return $ PVar $ E.Var $ Constr $ ("_" <> show index1)
binderToPat (ConstructorBinder _ _ (Qualified _ (ProperName p)) bs) = do
  let popName = unpack p
      popAtom = PLit $ LAtom $ Atom $ popName
  bs' <- mapM (binderToPat) bs
  return $ PTuple $ popAtom : (fmap (\(_, v) -> v) $ zip ([0 ..] :: [Integer]) bs')
binderToPat (NamedBinder _ i b) = do
  gs <- get
  let index1 = gs ^. binderVarIndex
  modify (\x -> x & localVar %~ M.insert (runIdent i) index1)
  modify (\x -> x & binderVarIndex %~ (+ 1))
  binderToPat b
binderToPat (MapBinder _ xs) = do
  xs' <- forM xs $ \(x, y) -> do
    x' <- binderToKey x
    y' <- binderToPat y
    return (x', y')
  return $ PMap $ Map xs'
binderToPat (BinaryBinder _ xs) = do
  xs' <- forM (addEnd xs) $ \(b, (x, y, z)) -> do
    x' <- binderToPat x
    (a, b1, c, d) <- paToC y z b
    return $ Bitstring x' [a, b1, c, d]
  return $ PBinary xs'
binderToPat (ListBinder _ xs b) = do
  xs' <- mapM binderToPat xs
  b' <- binderToPat b
  return $ PList (LL xs' b')


tText :: [Text] -> List Exprs
tText xs = E.L $ fmap (Expr . Constr . Lit . LAtom . Atom . unpack) $ cMark "big" endianness . cMark "unsigned" signedness $ fmap toLower xs

signedness :: [Text]
signedness = ["signed", "unsigned"]

endianness :: [Text]
endianness = ["big", "little", "native"]

cMark :: Text -> [Text] -> [Text] -> [Text]
cMark def ts input = case Prelude.filter id $ fmap (`elem` ts) input of
  [] -> def : input
  [_] -> input
  _ -> error $ show input

dealMI :: Maybe Integer -> [Text] -> (Exprs, Exprs, Exprs, Exprs)
dealMI Nothing ts =
  ( Expr (Constr $ Lit $ LInt 8),
    Expr $ Constr $ Lit $ LInt 1,
    Expr $ Constr $ Lit $ LAtom $ Atom "integer",
    Expr $ Constr $ E.List $ tText $ LL.delete "Integer" ts
  )
dealMI (Just i) ts =
  ( Expr (Constr $ Lit $ LInt i),
    Expr $ Constr $ Lit $ LInt 1,
    Expr $ Constr $ Lit $ LAtom $ Atom "integer",
    Expr $ Constr $ E.List $ tText $ LL.delete "Integer" ts
  )

data MType
  = N
  | B
  | I
  deriving (Show, Eq, Ord)

ddType :: [Text] -> MType
ddType xs =
  if "Integer" `elem` xs
    then I
    else
      if "Binary" `elem` xs
        then B
        else N

addEnd :: [a] -> [(Bool, a)]
addEnd [] = []
addEnd [x] = [(True, x)]
addEnd (x : xs) = (False, x) : addEnd xs

paToC :: Maybe Integer -> Maybe [Text] -> Bool -> Translate (Exprs, Exprs, Exprs, Exprs)
paToC mi ts b = case ts of
  Nothing -> return $ dealMI mi []
  Just ts' -> case ddType ts' of
    N -> return $ dealMI mi ts'
    I -> return $ dealMI mi ts'
    B -> case mi of
      Just i ->
        return $
          ( Expr $ Constr $ Lit $ LInt i,
            Expr $ Constr $ Lit $ LInt 8,
            Expr $ Constr $ Lit $ LAtom $ Atom "binary",
            Expr $ Constr $ E.List $ tText $ LL.delete "Binary" ts'
          )
      Nothing -> case b of
        False -> throwError "binary error "
        True ->
          return $
            ( Expr $ Constr $ Lit $ LAtom $ Atom "all",
              Expr $ Constr $ Lit $ LInt 8,
              Expr $ Constr $ Lit $ LAtom $ Atom "binary",
              Expr $ Constr $ E.List $ tText $ LL.delete "Binary" ts'
            )
tupleToBinaryVal :: (Integer, Integer) -> Bitstring Exprs
tupleToBinaryVal (a,b) = Bitstring (Expr $ Constr $ Lit $ LInt a)
                         [ Expr (Constr $ Lit $ LInt b),
                           Expr $ Constr $ Lit $ LInt 1,
                           Expr $ Constr $ Lit $ LAtom $ Atom "integer",
                           Expr $ Constr $ E.List $ tText $ LL.delete "Integer" []
                         ]

binderToKey :: Binder C.Ann -> Translate Key
binderToKey (LiteralBinder _ (NumericLiteral (Left i))) = return $ KLit $ LInt i
binderToKey (LiteralBinder _ (NumericLiteral (Right i))) = return $ KLit $ LFloat i
binderToKey (LiteralBinder _ (StringLiteral s)) = return $ KLit $ LString $ decodePPS s
binderToKey (LiteralBinder _ (AtomLiteral s)) = return $ KLit $ LAtom $ Atom $ decodePPS s
binderToKey (LiteralBinder _ (CharLiteral c)) = return $ KLit $ LChar c
binderToKey (LiteralBinder _ (BooleanLiteral True)) = return $ KLit $ LAtom (Atom "true")
binderToKey (LiteralBinder _ (BooleanLiteral False)) = return $ KLit $ LAtom (Atom "false")
binderToKey x = error $ show x

-- | Literal Binder to Pat
literalBinderToPat :: L.Literal (C.Binder C.Ann) -> Translate E.Pat
literalBinderToPat (NumericLiteral (Left i)) = return $ PLit $ LInt i
literalBinderToPat (NumericLiteral (Right i)) = return $ PLit $ LFloat i
literalBinderToPat (StringLiteral s) = return $ PLit $ LString $ decodePPS s
literalBinderToPat (AtomLiteral s) = return $ PLit $ LAtom $ Atom $ decodePPS s
literalBinderToPat (CharLiteral c) = return $ PLit $ LChar c
literalBinderToPat (BooleanLiteral True) = return $ PLit $ LAtom (Atom "true")
literalBinderToPat (BooleanLiteral False) = return $ PLit $ LAtom (Atom "false")
literalBinderToPat (ListLiteral xs) = do
  xs' <- mapM binderToPat xs
  return $ E.PList (L xs')
literalBinderToPat (TupleLiteral a b) = do
  a' <- binderToPat a
  b' <- binderToPat b
  return $ E.PTuple [a', b']
literalBinderToPat (TupleLiteral3 a b c) = do
  a' <- binderToPat a
  b' <- binderToPat b
  c' <- binderToPat c
  return $ E.PTuple [a', b', c']
literalBinderToPat (TupleLiteral4 a b c d) = do
  a' <- binderToPat a
  b' <- binderToPat b
  c' <- binderToPat c
  d' <- binderToPat d
  return $ E.PTuple [a', b', c', d']
literalBinderToPat (TupleLiteral5 a b c d e) = do
  a' <- binderToPat a
  b' <- binderToPat b
  c' <- binderToPat c
  d' <- binderToPat d
  e' <- binderToPat e
  return $ E.PTuple [a', b', c', d', e']
literalBinderToPat (TupleLiteral6 a b c d e f) = do
  a' <- binderToPat a
  b' <- binderToPat b
  c' <- binderToPat c
  d' <- binderToPat d
  e' <- binderToPat e
  f' <- binderToPat f
  return $ E.PTuple [a', b', c', d', e', f']
literalBinderToPat (TupleLiteral7 a b c d e f g) = do
  a' <- binderToPat a
  b' <- binderToPat b
  c' <- binderToPat c
  d' <- binderToPat d
  e' <- binderToPat e
  f' <- binderToPat f
  g' <- binderToPat g
  return $ E.PTuple [a', b', c', d', e', f', g']
literalBinderToPat (ObjectLiteral xs) = do
  xs' <- forM xs $ \(pps, e) -> do
    e' <- binderToPat e
    return (KLit $ LAtom $ Atom $ decodePPS pps, e')
  return $ PMap $ E.Map xs'
