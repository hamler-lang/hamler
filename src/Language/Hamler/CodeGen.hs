-----------------------------------------------------------------------------
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Hamler.CodeGen where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.List as LL
import Data.Map as M
import Data.Text (Text, toLower, unpack)
import qualified Data.Text as T
import Language.CoreErlang as E
import Language.Hamler.Util
import qualified Language.PureScript as P
import qualified Language.PureScript.AST.Literals as L
import Language.PureScript.CoreFn as C
import Language.PureScript.Names
import Prelude

data PF
  = Param (Var Text)
  | FunctionName Integer
  | LetFun (Var Text, Integer)
  | LetRecFun (FunName Text, Integer)
  deriving (Show)

type VarMap = M.Map Ident PF

data VarState = VarState Integer VarMap deriving (Show)

class Monad m => MonadVarState m where
  freshVar :: m (Var Text)
  lookVar :: Ident -> m (Maybe PF)
  insertParam :: Ident -> m (Var Text)
  insertFunctionName :: Ident -> Integer -> m ()
  insertLetFun :: Ident -> Integer -> m (Var Text)
  insertLetRecFun :: Ident -> Integer -> m (FunName Text)
  withVarState :: m a -> m a
  reset :: m ()

type Tain a = (Functor a, Applicative a, Monad a, MonadReader Tenv a, MonadState VarState a, MonadError P.MultipleErrors a, MonadWriter Text a, MonadVarState a)

type Tenv = (Maybe (E.Module Text), M.Map Text Int, M.Map Text (M.Map Text Integer), ModuleName)

newtype Translate a = Translate (ExceptT P.MultipleErrors (StateT VarState (ReaderT Tenv (Writer Text))) a)
  deriving (Functor, Applicative, Monad, MonadState VarState, MonadReader Tenv, MonadError P.MultipleErrors, MonadWriter Text)

runTranslate ::
  Bool ->
  (M.Map Text (M.Map Text Integer)) ->
  (Maybe (E.Module Text), ModuleName) ->
  Translate a ->
  (((Either P.MultipleErrors a), VarState), Text)
runTranslate _ moduleInfo (ffiModule, mn) (Translate translate) =
  runWriter $ runReaderT (runStateT (runExceptT translate) (VarState 0 M.empty)) (ffiModule, M.fromList $ moduleToFuns mn ffiModule, moduleInfo, mn)

instance MonadVarState Translate where
  freshVar = do
    VarState v m <- get
    put $ VarState (v + 1) m
    return (ann $ E.Var $ T.pack $ "_" <> show v)
  lookVar var = do
    VarState _ varMap <- get
    return $ M.lookup var varMap
  insertParam var = do
    VarState i m <- get
    put $ VarState (i + 1) $ M.insert var (Param $ ann $ E.Var $ T.pack $ "_" <> show i) m
    return . ann . E.Var . T.pack $ "_" <> show i
  insertFunctionName var i = modify' $ \(VarState n m) -> VarState n (M.insert var (FunctionName i) m)
  insertLetFun var v = do
    VarState i m <- get
    put $ VarState (i + 1) $ M.insert var (LetFun ((ann $ E.Var $ T.pack $ "_" <> show i), v)) m
    return . ann . E.Var . T.pack $ "_" <> show i
  insertLetRecFun var v = do
    let tfunName = ann $ FunName (ann $ Atom ("$LetRecFun_" <> runIdent var)) v
    modify'
      ( \(VarState i m) ->
          VarState i (M.insert var (LetRecFun (tfunName, v)) m)
      )
    return tfunName
  withVarState act = do
    VarState _ m <- get
    v <- act
    VarState i _ <- get
    put (VarState i m)
    return v
  reset = do
    VarState _ v <- get
    put $ VarState 0 v

exprToFunDef :: Integer -> FunName Text -> E.Expr Text -> FunDef Text
exprToFunDef 1 fn (EFun f _) = FunDef fn f
exprToFunDef 0 fn e = FunDef fn (ann $ Fun [] $ ann $ Expr e)
exprToFunDef _ _ _ = error "internal error"

exprToExpr :: Integer -> E.Expr Text -> E.Expr Text
exprToExpr 1 f = f
exprToExpr 0 e = ann $ EFun $ ann $ Fun [] $ ann $ Expr e
exprToExpr _ _ = error "internal error"

funArgs :: C.Expr C.Ann -> Integer
funArgs e = case e of
  Abs _ _ _ -> 1
  Constructor _ _ _ vs | length vs > 0 -> 1
  _ -> 0

moduleToFuns :: ModuleName -> Maybe (E.Module Text) -> [(Text, Int)]
moduleToFuns _ Nothing = []
moduleToFuns mn (Just (E.Module _ xs _ _ _)) = fmap g xs
  where
    g (FunName (Atom m _) i _) = (runModuleName mn <> "." <> m, fromInteger i)

moduleToFunDefs :: Maybe (E.Module Text) -> [FunDef Text]
moduleToFunDefs Nothing = []
moduleToFunDefs (Just (E.Module _ _ _ xs _)) = xs

moduleToErl :: Tain m => C.Module C.Ann -> m (E.Module Text)
moduleToErl C.Module {..} = do
  let tt = runModuleName moduleName
  funDecls <- concat <$> mapM bindToErl moduleDecls
  let funDecls' = moduleInfo0 tt : moduleInfo1 tt : funDecls
  (ffiModule, ffiFun, _, _) <- ask
  exports <- forM moduleExports $ \ident -> do
    let name = showQualified runIdent $ mkQualified ident moduleName
        wname = runIdent ident
    tvar <- lookVar ident
    case tvar of
      Just (FunctionName args) -> return . ann $ FunName (ann $ Atom wname) (toInteger args)
      Just _ -> error "internal error"
      Nothing -> case M.lookup name ffiFun of
        Just args ->
          return . ann $ FunName (ann $ Atom wname) (toInteger args)
        Nothing -> throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.MissingFFIImplementations moduleName [ident]]
  ffiDefs <- filterFunDef moduleName funDecls' $ moduleToFunDefs ffiModule
  return $
    ann $
      E.Module
        (ann $ Atom $ runModuleName moduleName)
        (mm1 : mm0 : exports)
        (ann $ Attrs [])
        (funDecls' ++ ffiDefs)

filterFunDef :: Tain m => ModuleName -> [FunDef Text] -> [FunDef Text] -> m [FunDef Text]
filterFunDef moduleName s source = do
  let nameSet = fmap (\(FunDef fn _) -> fn) s
      source' = Prelude.filter (\(FunDef fn _) -> not $ fn `elem` [mm0, mm1]) source
      v = Prelude.filter (\(FunDef fn _) -> not $ fn `elem` nameSet) source'
  if length v == length source'
    then return v
    else do
      let allDupFun =
            fmap (\(FunDef (FunName (Atom fn _) _ _) _) -> Ident fn) $
              Prelude.filter (\(FunDef fn _) -> fn `elem` nameSet) source'
      throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.FFIFunSameNameWithModule moduleName allDupFun]

bindToErl :: Tain m => C.Bind C.Ann -> m [FunDef Text]
bindToErl (NonRec _ ident e) = do
  let fi = funArgs e
  reset
  insertFunctionName ident fi
  e' <- withVarState $ exprToErl e
  return $ [exprToFunDef fi (ann $ FunName (ann $ Atom $ runIdent ident) fi) e']
bindToErl (C.Rec xs) = do
  mapM_ (\((_, a), b) -> insertFunctionName a (funArgs b)) xs
  forM xs $ \((_, a), b) -> withVarState $ do
    reset
    b' <- exprToErl b
    let fi = funArgs b
    return $ exprToFunDef fi (ann $ FunName (ann $ Atom $ runIdent a) fi) b'

letFunDef :: Tain m => C.Bind C.Ann -> m (Var Text, E.Expr Text)
letFunDef (NonRec _ ident e) = do
  let fi = funArgs e
  vn <- insertLetFun ident fi
  e' <- withVarState $ exprToErl e
  return $ (vn, exprToExpr fi e')
letFunDef x = error $ show x

letFunDefRec :: Tain m => C.Bind C.Ann -> m [FunDef Text]
letFunDefRec (C.Rec xs) = do
  vns <- mapM (\((_, a), b) -> insertLetRecFun a (funArgs b)) xs
  forM (zip vns xs) $ \(vn, ((_, _), b)) -> withVarState $ do
    let fi = funArgs b
    b' <- exprToErl b
    return $ exprToFunDef fi vn b'
letFunDefRec x = error $ show x

exprToErl :: Tain m => C.Expr C.Ann -> m (E.Expr Text)
exprToErl (Literal _ l) = literalToErl l
exprToErl (Constructor _ _ (ProperName p) ids) = do
  let constrName = ann . ELit . ann . LAtom . ann . Atom $ p :: E.Expr Text
  vars <- mapM insertParam ids
  return $ netConst constrName vars []
exprToErl (Accessor _ pps e) = do
  e' <- exprToErl e
  return . ann $ EModCall mapsAtom getAtom [ppsToAtomExprs pps, ann $ Expr e']
exprToErl (ObjectUpdate _ e xs) = do
  e' <- exprToErl e
  let foldFun m (k, v) = do
        v' <- exprToErl v
        return $ ann $ EModCall mapsAtom putAtom [ppsToAtomExprs k, ann $ Expr v', ann $ Expr m]
  foldM foldFun e' xs
exprToErl (Abs _ i e) = do
  var <- insertParam i
  e' <- exprToErl e
  return . ann . EFun . ann $ Fun [var] (ann $ Expr e')
exprToErl (C.App _ e1 e2) = do
  e1' <- exprToErl e1
  e2' <- exprToErl e2
  return . ann $ EApp (ann $ Expr e1') [ann $ Expr e2']
exprToErl (C.Var _ qi@(Qualified mdi ident)) = do
  (_, ffiMap, otherModule, mName) <- ask
  case mdi of
    Just mn | mn /= mName -> do
          let mn' = runModuleName mn
          case M.lookup mn' otherModule >>= M.lookup (showQualified runIdent qi) of
            Nothing ->
               if unpack mn' == "Prim" && unpack (runIdent ident) == "undefined"
               then return . ann . EFun . ann $ Fun [ann $ E.Var "_0"] (ann . Expr . ann . EVar . ann . E.Var $ "_0")
               else throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.MissingFFIImplementations mn [ident]]
            Just i -> cModCall (fromInteger i) mn' (runIdent ident)
    _ -> do
      lookVar ident >>= \case
        Just (Param v) -> return $ ann $ EVar v
        Just (FunctionName x) -> case x of
          0 -> return $ evalFunWithArgs0 ident
          _ -> return . ann . EFunN . ann $ E.FunName (ann $ Atom $ runIdent ident) 1
        Just (LetFun (v, x)) -> case x of
          0 -> return $ ann $ EApp (ann $ Expr $ ann $ EVar v) []
          _ -> return $ ann $ EVar v
        Just (LetRecFun (v, x)) -> case x of
          0 -> return $ ann $ EApp (ann $ Expr $ ann $ EFunN v) []
          _ -> return $ ann $ EFunN v
        Nothing -> case M.lookup (showQualified runIdent qi) ffiMap of
          Just 0 -> return $ evalFunWithArgs0 ident
          Just n -> do
            let ndd = ann . EFunN . ann $ FunName (ann $ Atom $ runIdent ident) (toInteger n)
            vars <- sequence $ replicate n freshVar
            return $ netLambda vars ndd []
          Nothing -> throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.MissingFFIImplementations mName [ident]]
exprToErl r@(C.Let _ _ _) = handleLetExpr r
exprToErl (C.Case _ es alts) = do
  es' <- mapM exprToErl es
  alts' <- dealAlts es' alts
  let len = length es'
  vars <- sequence $ replicate len freshVar
  return . ann $
    ELet
      vars
      (ann $ Exprs es')
      (ann . Expr . ann $ ECase (ann . E.Exprs $ fmap (ann . EVar) vars) alts')
exprToErl (C.Receive _ (Just (e1, e2)) alts) = do
  alts' <- mapM dealRecAlt alts
  e2' <- exprToErl e2
  return $ ann $ EFun $ ann $ Fun [] $ ann $ Expr $ recvExpr alts' (ann $ ELit $ ann $ LInt e1) e2'
exprToErl (C.Receive _ Nothing alts) = do
  alts' <- mapM dealRecAlt alts
  return $
    ann $ EFun $ ann $ 
          Fun [] $ ann $ 
              Expr $ recvExpr alts'
                  (ann $ ELit $ ann $ LAtom $ ann $ Atom "infinity")
                  (ann $ ELit $ ann $ LAtom $ ann $ Atom "true")
exprToErl (C.List _ es e) = do
  es' <- mapM exprToErl es
  e' <- exprToErl e
  return $ elist es' e'

appExpr :: E.Expr Text -> E.Expr Text
appExpr expr = ann $ EApp (ann $ Expr expr) []

matchFail :: Tain m => CaseAlternative C.Ann -> m [Clause Text]
matchFail ca =
  case Prelude.filter isBinaryBinder $ caseAlternativeBinders ca of
    [] -> return []
    _ -> do
      let len = length $ caseAlternativeBinders ca
      indexs0 <- mapM (\_ -> freshVar) [1 .. len]
      let indexs = fmap (\v -> ann $ PVar v) indexs0
          indexs' = fmap (\v -> ann $ Expr $ ann $ EVar v) indexs0
      return [ann . Clause indexs guardv $ aPrimop indexs']

isBinaryBinder :: Binder C.Ann -> Bool
isBinaryBinder (BinaryBinder _ _) = True
isBinaryBinder (MapBinder _ _) = True
isBinaryBinder _ = False

isWildBinder :: Binder C.Ann -> Bool
isWildBinder (NullBinder _) = True
isWildBinder (VarBinder _ _) = True
isWildBinder _ = False

guardv :: Exprs Text
guardv = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"

dealRecAlt :: Tain m => CaseAlternative C.Ann -> m (E.Clause Text)
dealRecAlt (CaseAlternative bs res) = do
  pats <- mapM binderToPat bs
  let guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"
  case res of
    Right expr -> do
      expr' <- exprToErl expr
      return
        . ann
        $ Clause pats guard1 (ann . Expr $ appExpr expr')
    Left xs -> error $ show xs

dealAlts :: Tain m => [E.Expr Text] -> [CaseAlternative C.Ann] -> m [Clause Text]
dealAlts _ [] = error "strange happened"
dealAlts _ alts@[(CaseAlternative bs res)] = do
  let guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true" :: Exprs Text
  pats <- mapM binderToPat bs
  case res of
    Right expr -> do
      expr' <- exprToErl expr
      tanV <- matchFail (head alts)
      return $ (ann $ Clause pats guard1 (ann . Expr $ expr')) : tanV
    Left x -> do
      x' <- guardToErl x
      return
        [ann $ Clause pats guard1 (ann $ Expr $ x')]
dealAlts es ((CaseAlternative bs res) : xs) = do
  let guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true" :: Exprs Text
  pats <- mapM binderToPat bs
  case res of
    Right expr -> do
      expr' <- exprToErl expr
      exprs' <- dealAlts es xs
      return $
        (ann $ Clause pats guard1 (ann . Expr $ expr')) : exprs'
    Left x -> do
      x' <- gdToErl x es xs
      exprs' <- do
        let t1 = length $ Prelude.filter isWildBinder bs
        if t1 == length bs
          then return []
          else dealAlts es xs
      return $ (ann $ Clause pats guard1 (ann $ Expr $ x')) : exprs'

gdToErl :: Tain m => [(C.Guard C.Ann, C.Expr C.Ann)] -> [E.Expr Text] -> [CaseAlternative C.Ann] -> m (E.Expr Text)
gdToErl [] _ _ = error "strange happened"
gdToErl [(g, e)] res rxs = do
  g' <- exprToErl g
  e' <- exprToErl e
  res' <- dealAlts res rxs
  let true = ann . PLiteral . ann . LAtom . ann $ Atom "true"
      false = ann . PLiteral . ann . LAtom . ann $ Atom "false"
      guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"
      altTrue = ann $ Clause [true] guard1 (ann $ Expr e')
      altFalse = ann $ Clause [false] guard1 . ann $ Expr (ann $ ECase (ann $ Exprs $ res) res')
  return . ann $ ECase (ann $ E.Expr g') [altTrue, altFalse]
gdToErl ((g, e) : xs) res rxs = do
  g' <- exprToErl g
  e' <- exprToErl e
  xs' <- gdToErl xs res rxs
  let true = ann . PLiteral . ann . LAtom . ann $ Atom "true"
      false = ann . PLiteral . ann . LAtom . ann $ Atom "false"
      guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"
      altTrue = ann $ Clause [true] guard1 (ann $ Expr e')
      altFalse = ann $ Clause [false] guard1 . ann $ Expr xs'
  return $ ann $ ECase (ann $ E.Expr g') [altTrue, altFalse]

guardToErl :: Tain m => [(C.Guard C.Ann, C.Expr C.Ann)] -> m (E.Expr Text)
guardToErl [] =
  return . ann $
    EModCall
      (stringToAtomExprs "erlang")
      (stringToAtomExprs "error")
      [ann $ Expr $ ann $ ELit $ ann $ LString "error"]
guardToErl ((g, e) : xs) = do
  g' <- exprToErl g
  e' <- exprToErl e
  xs' <- guardToErl xs
  let true = ann . PLiteral . ann . LAtom . ann $ Atom "true"
      false = ann . PLiteral . ann . LAtom . ann $ Atom "false"
      guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"
      altTrue = ann $ Clause [true] guard1 (ann $ Expr e')
      altFalse = ann $ Clause [false] guard1 . ann $ Expr xs'
  return $ ann $ ECase (ann $ E.Expr g') [altTrue, altFalse]

evalFunWithArgs0 :: Ident -> E.Expr Text
evalFunWithArgs0 ident = ann $ EApp (ann $ Expr $ ann $ EFunN $ ann $ FunName (ann $ Atom (runIdent ident)) 0) []

literalToErl :: Tain m => L.Literal (C.Expr C.Ann) -> m (E.Expr Text)
literalToErl (NumericLiteral (Left i)) = return . ann . ELit . ann $ LInt i
literalToErl (NumericLiteral (Right i)) = return . ann . ELit . ann $ LFloat i
literalToErl (StringLiteral s) = return . ann . ELit . ann . LString . T.pack $ decodePPS s
literalToErl (AtomLiteral s) = return . ann . ELit . ann . LAtom . ann . Atom . T.pack $ decodePPS s
literalToErl (CharLiteral c) = return . ann . ELit . ann $ LChar c
literalToErl (BooleanLiteral True) = return . ann . ELit . ann $ LAtom (ann $ Atom "true")
literalToErl (BooleanLiteral False) = return . ann . ELit . ann $ LAtom (ann $ Atom "false")
literalToErl (ListLiteral xs) = do
  xs' <- mapM exprToErl xs
  return $ elist' xs'
literalToErl (TupleLiteral xs) = do
  xs' <- mapM exprToErl xs
  return . ann . ETuple $ fmap (ann . Expr) xs'
literalToErl (Tuple2Literal a b) = do
  a' <- exprToErl a
  b' <- exprToErl b
  return . ann . ETuple $ fmap (ann . Expr) [a', b']
literalToErl (ObjectLiteral xs) = do
  xs' <- forM xs $ \(pps, e) -> do
    e' <- exprToErl e
    return $
      ann $
        Insert
          (ppsToAtomExprs pps)
          (ann $ Expr e')
  return . ann . EMap $ E.Map xs'
literalToErl (BinaryLiteral xs) = return . ann . EBinary $ fmap tupleToBinaryVal xs

tText :: [Text] -> Pat Text
tText xs =
  elistpat' $
    fmap (ann . PLiteral . ann . LAtom . ann . Atom) $
      cMark "big" endianness . cMark "unsigned" signedness $ fmap toLower xs

tText' :: [Text] -> E.Expr Text
tText' xs =
  elist' $
    fmap (ann . ELit . ann . LAtom . ann . Atom) $
      cMark "big" endianness . cMark "unsigned" signedness $ fmap toLower xs

signedness :: [Text]
signedness = ["signed", "unsigned"]

endianness :: [Text]
endianness = ["big", "little", "native"]

cMark :: Text -> [Text] -> [Text] -> [Text]
cMark def ts input = case Prelude.filter id $ fmap (`elem` ts) input of
  [] -> def : input
  [_] -> input
  _ -> error $ show input

dealMI :: Maybe Integer -> [Text] -> (Pat Text, Pat Text, Pat Text, Pat Text)
dealMI Nothing ts =
  ( ann . PLiteral . ann $ LInt 8,
    ann . PLiteral . ann $ LInt 1,
    ann . PLiteral . ann . LAtom . ann $ Atom "integer",
    tText $ LL.delete "integer" ts
  )
dealMI (Just i) ts =
  ( ann . PLiteral . ann $ LInt i,
    ann . PLiteral . ann $ LInt 1,
    ann . PLiteral . ann . LAtom . ann $ Atom "integer",
    tText $ LL.delete "integer" ts
  )

data MType = N | B | I deriving (Show, Eq, Ord)

ddType :: [Text] -> MType
ddType xs =
  if "integer" `elem` xs
    then I
    else
      if "binary" `elem` xs
        then B
        else N

addEnd :: [a] -> [(Bool, a)]
addEnd [] = []
addEnd [x] = [(True, x)]
addEnd (x : xs) = (False, x) : addEnd xs

paToC :: Tain m => Maybe Integer -> Maybe [Text] -> Bool -> m (Pat Text, Pat Text, Pat Text, Pat Text)
paToC mi ts b = case ts of
  Nothing -> return $ dealMI mi []
  Just ts' -> case ddType ts' of
    N -> return $ dealMI mi ts'
    I -> return $ dealMI mi ts'
    B -> case mi of
      Just i ->
        return $
          ( ann . PLiteral . ann $ LInt i,
            ann . PLiteral . ann $ LInt 8,
            ann . PLiteral . ann $ LAtom . ann $ Atom "binary",
            tText $ LL.delete "binary" ts'
          )
      Nothing -> case b of
        False -> throwError . error $ "unsupport binary syntax" <> show b
        True ->
          return $
            ( ann . PLiteral . ann . LAtom . ann $ Atom "all",
              ann . PLiteral . ann $ LInt 8,
              ann . PLiteral . ann . LAtom . ann $ Atom "binary",
              tText $ LL.delete "binary" ts'
            )

tupleToBinaryVal :: (Integer, Integer) -> Bitstring (Exprs Text) Text
tupleToBinaryVal (a, b) =
  ann $
    Bitstring
      (ann . Expr . ann . ELit . ann $ LInt a)
      (ann . Expr . ann . ELit . ann $ LInt b)
      (ann . Expr . ann . ELit . ann $ LInt 1)
      (ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "integer")
      (ann . Expr . tText' $ LL.delete "integer" [])

netConst :: E.Expr Text -> [Var Text] -> [Var Text] -> E.Expr Text
netConst c [] [] = ann $ ETuple [ann $ Expr c]
netConst c [x] s = ann . EFun . ann $ Fun [x] (ann . Expr . ann . ETuple $ (ann $ Expr c) : fmap (ann . Expr . ann . EVar) (reverse $ x : s))
netConst c (x : xs) s = ann . EFun . ann $ Fun [x] (ann . Expr $ netConst c xs (x : s))
netConst _ _ _ = error "strange happend"

netLambda :: [Var Text] -> E.Expr Text -> [Var Text] -> E.Expr Text
netLambda [] _ [] = error "internal error"
netLambda [x] e s = ann . EFun . ann $ Fun [x] (ann . Expr . ann $ EApp (ann $ Expr e) (fmap (ann . Expr . ann . EVar) (reverse $ x : s)))
netLambda (x : xs) e s = ann . EFun . ann $ Fun [x] (ann . Expr $ netLambda xs e (x : s))
netLambda _ _ _ = error "strange happend"

handleLetExpr :: Tain m => C.Expr C.Ann -> m (E.Expr Text)
handleLetExpr (C.Let _ [] _) = error "internal error"
handleLetExpr (C.Let _ [x] e) = do
  case x of
    r@(NonRec _ _ _) -> do
      (vsr, expr) <- letFunDef r
      e' <- exprToErl e
      return $ ann $ ELet [vsr] (ann $ Expr expr) (ann $ Expr $ e')
    r@(Rec _) -> do
      funDefs <- letFunDefRec r
      e' <- exprToErl e
      return $ ann $ ELetRec funDefs $ ann $ Expr $ e'
handleLetExpr (C.Let a (x : xs) e) = do
  case x of
    r@(NonRec _ _ _) -> do
      (vsr, expr) <- letFunDef r
      revs <- handleLetExpr (C.Let a xs e)
      return $ ann $ ELet [vsr] (ann $ Expr expr) (ann $ Expr revs)
    r@(Rec _) -> do
      funDefs <- letFunDefRec r
      revs <- handleLetExpr (C.Let a xs e)
      return $ ann $ ELetRec funDefs $ ann $ Expr revs
handleLetExpr _ = error "internal error"

binderToPat :: Tain m => C.Binder C.Ann -> m (E.Pat Text)
binderToPat (NullBinder _) = do
  var <- freshVar
  return $ ann $ PVar var
binderToPat (LiteralBinder _ l) = literalBinderToPat l
binderToPat (VarBinder _ i) = do
  var <- insertParam i
  return $ ann $ PVar var
binderToPat (ConstructorBinder _ _ (Qualified _ (ProperName p)) bs) = do
  let popName = ann . PLiteral . ann . LAtom . ann $ Atom p :: Pat Text
  bs' <- mapM binderToPat bs
  return . ann . PTuple $ popName : bs'
binderToPat (NamedBinder _ i b) = do
  var <- insertParam i
  b' <- binderToPat b
  return $ ann $ PAlias var b'
binderToPat (MapBinder _ xs) = do
  xs' <- forM xs $ \(x, y) -> do
    x' <- binderToPat x
    y' <- binderToPat y
    return . ann $ Update x' y'
  return . ann . PMap $ Map xs'
binderToPat (BinaryBinder _ xs) = do
  xs' <- forM (addEnd xs) $ \(b, (x, y, z)) -> do
    x' <- binderToPat x
    (a, b1, c, d) <- paToC y z b
    return . ann $ Bitstring x' a b1 c d
  return . ann $ PBinary xs'
binderToPat (ListBinder _ xs b) = do
  xs' <- mapM binderToPat xs
  b' <- binderToPat b
  return $ elistpat xs' b'

literalBinderToPat :: Tain m => L.Literal (C.Binder C.Ann) -> m (E.Pat Text)
literalBinderToPat (NumericLiteral (Left i)) = return . ann . PLiteral . ann $ LInt i
literalBinderToPat (NumericLiteral (Right i)) = return . ann . PLiteral . ann $ LFloat i
literalBinderToPat (StringLiteral s) = return . ann . PLiteral . ann . LString . T.pack $ decodePPS s
literalBinderToPat (AtomLiteral s) = return . ann . PLiteral . ann . LAtom . ann . Atom . T.pack $ decodePPS s
literalBinderToPat (CharLiteral c) = return . ann . PLiteral . ann $ LChar c
literalBinderToPat (BooleanLiteral True) = return . ann . PLiteral . ann $ LAtom (ann $ Atom "true")
literalBinderToPat (BooleanLiteral False) = return . ann . PLiteral . ann $ LAtom (ann $ Atom "false")
literalBinderToPat (ListLiteral xs) = do
  xs' <- mapM binderToPat xs
  return $ elistpat' xs'
literalBinderToPat (TupleLiteral xs) = do
  xs' <- mapM binderToPat xs
  return . ann $ E.PTuple xs'
literalBinderToPat (Tuple2Literal a b) = do
  a' <- binderToPat a
  b' <- binderToPat b
  return . ann $ E.PTuple [a', b']
literalBinderToPat (ObjectLiteral xs) = do
  xs' <- forM xs $ \(pps, e) -> do
    e' <- binderToPat e
    return . ann $ Update (ann . PLiteral . ann . LAtom . ann . Atom . T.pack $ decodePPS pps) e'
  return $ ann $ PMap $ E.Map xs'
literalBinderToPat x = error $ show x

cModCall :: Tain m => Int -> Text -> Text -> m ( E.Expr Text)
cModCall 0 s1 s2 = return $ ann $ EModCall (stringToAtomExprs s1) (stringToAtomExprs s2) []
cModCall n s1 s2 = do 
  vars <- sequence $ replicate n freshVar
  return $ netLambda1 vars [] (stringToAtomExprs s1) (stringToAtomExprs s2)

netLambda1 :: [Var Text] -> [Var Text] -> E.Exprs Text -> E.Exprs Text -> (E.Expr Text)
netLambda1 [] [] _ _ = error "strange error "
netLambda1 [x] s p1 p2 =
  ann . EFun . ann $ Fun [x] (ann . Expr . ann $ EModCall p1 p2 (fmap (ann . Expr . ann . EVar) (reverse $ x : s)))
netLambda1 (x : xs) s p1 p2 =
  ann . EFun . ann $ Fun [x] (ann . Expr $ netLambda1 xs (x : s) p1 p2)
netLambda1 _ _ _ _ = error "stringe error"

