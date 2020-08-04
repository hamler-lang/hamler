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
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Language.CoreErlang as E
import Language.Hamler.Util
import qualified Language.PureScript.AST.Literals as L
import Language.PureScript.CoreFn as C
import qualified Language.PureScript as P
import Language.PureScript.Names
import Text.Pretty.Simple
import Prelude

type MError = P.MultipleErrors

type MLog = Text

type Translate = ExceptT MError (StateT GState (Writer MLog))

data GState = GState
  { _gsmoduleName :: ModuleName,
    _ffiFun :: M.Map Text Int,
    _globalVar :: M.Map Text Int,
    _localVar :: M.Map Text Int,
    _letMap :: M.Map Text (Int, E.Expr Text),
    _binderVarIndex :: Int,
    _modInfoMap :: M.Map Text (M.Map Text Int),
    _isInline :: Bool
  }
  deriving (Show)

makeLenses ''GState

emptyGState :: GState
emptyGState = GState (ModuleName []) M.empty M.empty M.empty M.empty 0 M.empty False

moduleToFuns :: ModuleName -> Maybe (E.Module Text) -> [(Text, Int)]
moduleToFuns _ Nothing = []
moduleToFuns mn (Just (E.Module _ xs _ _ _)) = fmap g xs
  where
    g (FunName (Atom m _) i _) = (runModuleName mn <> "." <> m, fromInteger i)

moduleToFunDefs :: Maybe (E.Module Text) -> [FunDef Text]
moduleToFunDefs Nothing = []
moduleToFunDefs (Just (E.Module _ _ _ xs _)) = xs

runTranslate ::
  Bool ->
  (M.Map Text (M.Map Text Int)) ->
  (Maybe (E.Module Text), ModuleName) ->
  Translate a ->
  (((Either MError a), GState), MLog)
runTranslate isinline modinfos (ffiModule, mn) translate =
  runWriter $
    runStateT
      (runExceptT translate)
      ( (emptyGState & ffiFun .~ (M.fromList $ moduleToFuns mn ffiModule))
          & modInfoMap .~ modinfos
          & isInline .~ isinline
      )

-- | CoreFn Module to CoreErlang AST
moduleToErl :: C.Module C.Ann -> Maybe (E.Module Text) -> Translate (E.Module Text)
moduleToErl C.Module {..} ffiModule = do
  modify (\x -> x & gsmoduleName .~ moduleName)
  let tt = runModuleName moduleName
  funDecls <- concat <$> mapM bindToErl moduleDecls
  let funDecls' = moduleInfo0 tt : moduleInfo1 tt : funDecls
  gs <- get
  exports <- forM moduleExports $ \ident -> do
    let name = showQualified runIdent $ mkQualified ident (gs ^. gsmoduleName)
        wname = runIdent ident
    case M.lookup name (gs ^. globalVar) of
      Just args -> return . ann $ FunName (ann $ Atom wname) (toInteger args)
      Nothing -> case M.lookup name (gs ^. ffiFun) of
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

filterFunDef :: ModuleName -> [FunDef Text] -> [FunDef Text] -> Translate [FunDef Text]
filterFunDef moduleName  s source = do
  let nameSet = fmap (\(FunDef fn _) -> fn) s
      source' = Prelude.filter (\(FunDef fn _) -> not $ fn `elem` [mm0, mm1]) source
      v = Prelude.filter (\(FunDef fn _) -> not $ fn `elem` nameSet) source'
  if length v == length source'
    then return v
    else do
    let allDupFun =fmap (\(FunDef (FunName (Atom fn _) _ _) _) -> Ident fn)
                    $ Prelude.filter (\(FunDef fn _) -> fn `elem` nameSet) source'
    throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.FFIFunSameNameWithModule moduleName allDupFun]

-- | CoreFn Bind to CoreErlang FunDef
bindToErl :: C.Bind C.Ann -> Translate [FunDef Text]
bindToErl (NonRec _ ident e) = do
  modify (\x -> x & binderVarIndex .~ 100)
  e' <- exprToErl e
  modify (\x -> x & localVar .~ M.empty)
  gs <- get
  let name = showQualified runIdent $ mkQualified ident (gs ^. gsmoduleName)
      wname = runIdent ident
  case e' of
    EFun v@(Fun [] (Expr (ELetRec _ _ "[\'letrec_goto\']") _) _) _ -> do
      modify (\x -> x & globalVar %~ M.insert name 0)
      return $ [FunDef (ann $ FunName (ann $ Atom wname) 0) (ann $ Fun [] (ann $ Expr $ ann $ EFun v))]
    EFun v@(Fun vrs _ _) _ -> do
      modify (\x -> x & globalVar %~ M.insert name (length vrs))
      return $ [FunDef (ann $ FunName (ann $ Atom wname) (toInteger $ length vrs)) v]
    _ -> do
      modify (\x -> x & globalVar %~ M.insert name 0)
      return $
        [ FunDef
            (ann $ FunName (ann $ Atom wname) 0)
            (ann $ Fun [] (ann $ Expr e'))
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
    EFun (Fun vrs _ _) _ -> do
      modify (\x -> x & letMap %~ M.insert name (length vrs, e'))
    _ -> do
      modify (\x -> x & letMap %~ M.insert name (0, ann . EFun . ann $ Fun [] (ann $ Expr e')))
bindToLetFunDef x = error $ L.unpack $ pShow x

-- | CoreFn Expr to CoreErlang Expr
exprToErl :: C.Expr C.Ann -> Translate (E.Expr Text)
exprToErl (Literal _ l) = literalToErl l
exprToErl (Constructor _ _ (ProperName p) ids) = do
  let args = length ids
      popAtom = ann . ELit . ann . LAtom . ann . Atom $ p
      cvar i = ann . E.Var $ T.pack ("_" <> show i)
      vars = fmap cvar [0 .. args -1]
  return $ netConst (popAtom) vars []
exprToErl (Accessor _ pps e) = do
  e' <- exprToErl e
  return . ann $ EModCall mapsAtom getAtom [ppsToAtomExprs pps, ann $ Expr e']
exprToErl (ObjectUpdate _ e xs) = do
  e' <- exprToErl e
  let foldFun m (k, v) = do
        v' <- exprToErl v
        return
          . ann
          $ EModCall
            mapsAtom
            putAtom
            [ppsToAtomExprs k, ann $ Expr v', ann $ Expr m]
  foldM foldFun e' xs
exprToErl (Abs _ i e) = do
  gs <- get
  let allVar = M.size (gs ^. localVar)
      var = ann . E.Var $ T.pack ("_" <> show allVar)
  modify (\x -> x & localVar %~ M.insert (runIdent i) allVar)
  e' <- exprToErl e
  put gs
  return . ann . EFun . ann $ Fun [var] (ann $ Expr e')
exprToErl (C.App _ e1@(C.App _ _ _) e2) = do
  e1' <- exprToErl e1
  e2' <- exprToErl e2
  gs <- get
  let allVar = M.size (gs ^. localVar)
      var = ann $ E.Var $ T.pack $ ("_" <> show allVar)
  modify (\x -> x & localVar %~ M.insert ("letBinderVar" <> (T.pack $ show allVar)) allVar)
  return . ann $ ELet [var] (ann $ Expr e1') (ann . Expr . ann $ EApp (ann . Expr . ann $ EVar var) [ann $ Expr e2'])
exprToErl (C.App _ e1 e2) = do
  e1' <- exprToErl e1
  e2' <- exprToErl e2
  return . ann $ EApp (ann $ Expr e1') [ann $ Expr e2']
exprToErl (C.Var _ qi@(Qualified _ tema)) = do
  let name = showQualified runIdent qi
      wname = runIdent tema
  gs <- get
  case M.lookup name (gs ^. localVar) of
    Just i -> return . ann . E.EVar . ann . E.Var $ T.pack ("_" <> show i)
    Nothing -> case M.lookup name (gs ^. letMap) of
      Just (0, e) -> return . ann $ EApp (ann $ Expr e) []
      Just (_, e) -> return e
      Nothing -> case M.lookup name (gs ^. globalVar) of
        Just a -> case a of
          0 -> return . ann $ EApp (ann $ Expr $ ann $ EFunN $ ann $ FunName (ann $ Atom wname) 0) []
          _ -> return . ann . EFunN . ann $ E.FunName (ann $ Atom wname) 1
        Nothing -> case M.lookup name (gs ^. ffiFun) of
          Just 0 -> return . ann $ EApp (ann . Expr . ann . EFunN . ann $ E.FunName (ann $ Atom wname) 0) []
          Just x -> do
            let ndd = ann . EFunN . ann $ FunName (ann $ Atom wname) (toInteger x)
                vars = fmap (\k -> ann $ E.Var $ T.pack ("_" <> show k)) [0 .. x -1]
            return $ netLambda vars ndd []
          Nothing -> case qi of
            Qualified (Just mn) _ -> do
              let mn' = runModuleName mn
                  funName = showQualified runIdent qi
                  res = do
                    r1 <- M.lookup mn' (gs ^. modInfoMap)
                    M.lookup funName r1
              case res of
                Nothing ->
                  if unpack mn' == "Prim" && unpack wname == "undefined"
                    then return . ann . EFun . ann $ Fun [ann $ E.Var "_0"] (ann . Expr . ann . EVar . ann . E.Var $ "_0")
                    else throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.MissingFFIImplementations mn [tema]]
                Just i -> return $ cModCall i mn' wname
            Qualified Nothing _ ->
              throwError $ P.MultipleErrors [P.ErrorMessage [] $ P.MissingFFIImplementations (gs ^. gsmoduleName) [tema]]
exprToErl (C.Let _ bs e) = do
  mapM_ bindToLetFunDef bs
  e' <- exprToErl e
  return e'
exprToErl (C.Case _ es alts) = do
  es' <- mapM exprToErl es
  alts' <- dealAlts es' alts
  gs <- get
  let allVar = M.size (gs ^. localVar)
      is = [allVar .. allVar + length es' -1]
      vars = fmap (\i -> ann . E.Var $ T.pack ("_" <> show i)) is
      varName = fmap (\i -> ("CaseVar" <> (T.pack $ show i))) is
  forM_ (zip varName is) $ \(n, i) -> do
    modify (\x -> x & localVar %~ M.insert n i)
  return . ann $ ELet vars (ann $ Exprs es') (ann . Expr . ann $ ECase (ann . E.Exprs $ fmap (ann . EVar) vars) alts')
exprToErl (C.Receive _ (Just (e1, e2)) alts) = do
  alts' <- mapM dealRecAlt alts
  e2' <- exprToErl e2
  return $ ann $ EFun $ ann $ Fun [] $ ann $ Expr $ recvExpr alts' (ann $ ELit $ ann $ LInt e1) e2'
exprToErl (C.Receive _ Nothing alts) = do
  alts' <- mapM dealRecAlt alts
  return $ ann $ EFun $ ann $ Fun [] $ ann $ Expr $ recvExpr alts' (ann $ ELit $ ann $ LAtom $ ann $ Atom "infinity")
                                                            (ann $ ELit $ ann $ LAtom $ ann $ Atom "true")
exprToErl (C.List _ es e) = do
  es' <- mapM exprToErl es
  e' <- exprToErl e
  return $ elist es' e'

appExpr :: E.Expr Text -> E.Expr Text
appExpr expr = ann $ EApp (ann $ Expr expr) []

matchFail :: CaseAlternative C.Ann -> Translate [Clause Text]
matchFail ca =
  case Prelude.filter isBinaryBinder $ caseAlternativeBinders ca of
    [] -> return []
    _ -> do
      gs <- get
      let len = length $ caseAlternativeBinders ca
      let index1 = gs ^. binderVarIndex
          indexs = fmap (\v -> ann . PVar . ann . E.Var . T.pack $ "_" <> show v) [index1 .. index1 + len -1]
          indexs' = fmap (\v -> ann . Expr . ann . EVar . ann . E.Var . T.pack $ "_" <> show v) [index1 .. index1 + len -1]
      modify (\x -> x & binderVarIndex %~ (+ len))
      return [ann . Clause indexs guardv $ aPrimop indexs']

isBinaryBinder :: Binder C.Ann -> Bool
isBinaryBinder (BinaryBinder _ _) = True
isBinaryBinder (MapBinder _ _) = True
isBinaryBinder _ = False

isWildBinder :: Binder C.Ann -> Bool
isWildBinder (NullBinder  _) = True
isWildBinder (VarBinder _ _) = True
isWildBinder _ = False


guardv :: Exprs Text
guardv = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"

dealRecAlt :: CaseAlternative C.Ann -> Translate (E.Clause Text)
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
      -- xs' <- guardToErl xs
      -- return
      --   . ann
      --   $ Clause pats guard1 (ann $ Exprs [xs'])

dealAlts :: [E.Expr Text] -> [CaseAlternative C.Ann] -> Translate [Clause Text]
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
dealAlts es ((CaseAlternative bs res):xs) = do
  let guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true" :: Exprs Text
  pats <- mapM binderToPat bs
  case res of
    Right expr -> do
      expr' <- exprToErl expr
      exprs' <- dealAlts es xs
      return
        $  (ann $ Clause pats guard1 (ann . Expr $ expr')) : exprs'
    Left x -> do
      x' <- gdToErl x  es xs
      exprs' <- do
        let t1 = length $ Prelude.filter isWildBinder bs
        if t1 == length bs
          then return []
          else dealAlts es xs
      return $ (ann $ Clause pats guard1 (ann $ Expr $ x')) : exprs'

gdToErl :: [(C.Guard C.Ann, C.Expr C.Ann)] -> [E.Expr Text] -> [CaseAlternative C.Ann] -> Translate (E.Expr Text)
gdToErl [] _ _ = error "strange happened"
gdToErl [(g, e)] res rxs= do
  g' <- exprToErl g
  e' <- exprToErl e
  res' <- dealAlts res rxs
  let true = ann . PLiteral . ann . LAtom . ann $ Atom "true"
      false = ann . PLiteral . ann . LAtom . ann $ Atom "false"
      guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"
      altTrue = ann $ Clause [true] guard1 (ann $ Expr e')
      altFalse = ann $ Clause [false] guard1 . ann $ Expr (ann $ ECase (ann $ Exprs $ res) res')
  return . ann $ ECase (ann $ E.Expr g') [altTrue, altFalse]
gdToErl ((g, e) : xs) res rxs= do
  g' <- exprToErl g
  e' <- exprToErl e
  xs' <- gdToErl xs res rxs
  let true = ann . PLiteral . ann . LAtom . ann $ Atom "true"
      false = ann . PLiteral . ann . LAtom . ann $ Atom "false"
      guard1 = ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "true"
      altTrue = ann $ Clause [true] guard1 (ann $ Expr e')
      altFalse = ann $ Clause [false] guard1 . ann $ Expr xs'
  return $ ann $ ECase (ann $ E.Expr g') [altTrue, altFalse]

guardToErl :: [(C.Guard C.Ann, C.Expr C.Ann)] -> Translate (E.Expr Text)
guardToErl [] = return . ann
  $ EModCall (stringToAtomExprs "erlang")
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

-- | CoreFn Literal to CoreErlang Expr
literalToErl :: L.Literal (C.Expr C.Ann) -> Translate (E.Expr Text)
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

-- | Binder to Pat
binderToPat :: Binder C.Ann -> Translate (Pat Text)
binderToPat (NullBinder _) = do
  gs <- get
  let i = gs ^. binderVarIndex
  modify (\x -> x & binderVarIndex %~ (+ 1))
  return . ann . PVar . ann . E.Var . T.pack $ "_" <> show i
binderToPat (LiteralBinder _ l) = literalBinderToPat l
binderToPat (VarBinder _ i) = do
  gs <- get
  let index1 = gs ^. binderVarIndex
  modify (\x -> x & localVar %~ M.insert (runIdent i) index1)
  modify (\x -> x & binderVarIndex %~ (+ 1))
  return . ann . PVar . ann . E.Var $ T.pack ("_" <> show index1)
binderToPat (ConstructorBinder _ _ (Qualified _ (ProperName p)) bs) = do
  let popName = unpack p
      popAtom = ann . PLiteral . ann . LAtom . ann . Atom $ T.pack popName
  bs' <- mapM (binderToPat) bs
  return . ann . PTuple $ popAtom : (fmap (\(_, v) -> v) $ zip ([0 ..] :: [Integer]) bs')
binderToPat (NamedBinder _ i b) = do
  gs <- get
  let index1 = gs ^. binderVarIndex
  modify (\x -> x & localVar %~ M.insert (runIdent i) index1)
  modify (\x -> x & binderVarIndex %~ (+ 1))
  b' <- binderToPat b
  return . ann $ PAlias ( ann $ E.Var $ T.pack $ "_" <> show index1) b'
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
    tText $ LL.delete "Integer" ts
  )
dealMI (Just i) ts =
  ( ann . PLiteral . ann $ LInt i,
    ann . PLiteral . ann $ LInt 1,
    ann . PLiteral . ann . LAtom . ann $ Atom "integer",
    tText $ LL.delete "Integer" ts
  )

data MType = N | B | I deriving (Show, Eq, Ord)

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

paToC :: Maybe Integer -> Maybe [Text] -> Bool -> Translate (Pat Text, Pat Text, Pat Text, Pat Text)
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
            tText $ LL.delete "Binary" ts'
          )
      Nothing -> case b of
        False -> throwError . error $ "unsupport binary syntax" <> show b
        True ->
          return $
            ( ann . PLiteral . ann . LAtom . ann $ Atom "all",
              ann . PLiteral . ann $ LInt 8,
              ann . PLiteral . ann . LAtom . ann $ Atom "binary",
              tText $ LL.delete "Binary" ts'
            )

tupleToBinaryVal :: (Integer, Integer) -> Bitstring (Exprs Text) Text
tupleToBinaryVal (a, b) =
  ann $
    Bitstring
      (ann . Expr . ann . ELit . ann $ LInt a)
      (ann . Expr . ann . ELit . ann $ LInt b)
      (ann . Expr . ann . ELit . ann $ LInt 1)
      (ann . Expr . ann . ELit . ann . LAtom . ann $ Atom "integer")
      (ann . Expr . tText' $ LL.delete "Integer" [])


-- | Literal Binder to Pat
literalBinderToPat :: L.Literal (C.Binder C.Ann) -> Translate (E.Pat Text)
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
literalBinderToPat (ObjectLiteral xs) = do
  xs' <- forM xs $ \(pps, e) -> do
    e' <- binderToPat e
    return . ann $ Insert (ann . PLiteral . ann . LAtom . ann . Atom . T.pack $ decodePPS pps) e'
  return $ ann $ PMap $ E.Map xs'
literalBinderToPat x = error $ show x

netLambda :: [Var Text] -> E.Expr Text -> [Var Text] -> E.Expr Text
netLambda [] _ [] = error "nice"
netLambda [x] e s = ann . EFun . ann $ Fun [x] (ann . Expr . ann $ EApp (ann $ Expr e) (fmap (ann . Expr . ann . EVar) (reverse $ x : s)))
netLambda (x : xs) e s = ann . EFun . ann $ Fun [x] (ann . Expr $ netLambda xs e (x : s))
netLambda _ _ _ = error "strange happend"

netConst :: E.Expr Text -> [Var Text] -> [Var Text] -> E.Expr Text
netConst c [] [] = ann $ ETuple [ann $ Expr c]
netConst c [x] s = ann . EFun . ann $ Fun [x] (ann . Expr . ann . ETuple $ (ann $ Expr c) : fmap (ann . Expr . ann . EVar) (reverse $ x : s))
netConst c (x : xs) s = ann . EFun . ann $ Fun [x] (ann . Expr $ netConst c xs (x : s))
netConst _ _ _ = error "strange happend"
