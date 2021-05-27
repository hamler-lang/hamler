{-# LANGUAGE PatternSynonyms #-}

module Language.Hamler.Erlang.TranslateSimpleType where

import qualified Data.Bifunctor
import Data.Char
import Language.Hamler.Erlang.SimpleType as S
import Erlang.Type as E
import Prelude

toFormsN :: String -> [(String, Bool)] -> [(String, S.Expr)] -> Forms
toFormsN mn exports decls = listToForms (mnF : expF : declsF)
  where
    mnF =
      Form0 $
        Attribute0
          (Atom "module")
          ( AttrVal0
              ( Expr13 $
                  ExprRemote1 $
                    ExprMax7 $
                      Expr13 $
                        ExprRemote1 $ ExprMax1 $ Atomic3 $ Atom mn
              )
          )
    expF =
      Form0 $
        Attribute0
          (Atom "export")
          ( AttrVal0
              ( Expr13 $
                  ExprRemote1 $
                    ExprMax7 $
                      Expr13 $
                        ExprRemote1 $
                          ExprMax2 $
                            listToList'
                              (map (\(n, b) -> Expr8 (atomToExpr n) Mx (intToExpr $ if b then 1 else 0)) exports)
              )
          )
    declsF = map (uncurry toForm) decls

pattern ExportList :: List -> Form
pattern ExportList l =
  Form0
    ( Attribute0
        (Atom "export")
        ( AttrVal0
            ( Expr13
                ( ExprRemote1
                    (ExprMax7 (Expr13 (ExprRemote1 (ExprMax2 l))))
                  )
              )
          )
      )

listToDec :: List -> [E.Expr]
listToDec List0 = []
listToDec (List1 x xs) = x : go xs
  where
    go (Tail2 v ls) = v : go ls
    go Tail0 = []

exprToDec :: E.Expr -> (String, Integer)
exprToDec
  ( Expr8
      (Expr13 (ExprRemote1 (ExprMax1 (Atomic3 (Atom a)))))
      Mx
      (Expr13 (ExprRemote1 (ExprMax1 (Atomic1 i))))
    ) = (a, i)
exprToDec _ = error "never happend"

dec :: List -> [(String, Integer)]
dec l = map exprToDec $ listToDec l

formsToList :: Forms -> [Form]
formsToList (Forms0 x) = [x]
formsToList (Forms1 x ls) = x : formsToList ls

atomToExpr :: String -> E.Expr
atomToExpr s = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic3 $ Atom s

intToExpr :: Integer -> E.Expr
intToExpr i = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic1 i

listToForms :: [Form] -> Forms
listToForms [] = error "never happend"
listToForms [x] = Forms0 x
listToForms (x : xs) = Forms1 x (listToForms xs)

toForms :: [(String, S.Expr)] -> Forms
toForms [] = Forms0 $ Form0 (Attribute0 (Atom "nothing") (AttrVal0 (Expr13 (ExprRemote1 (ExprMax0 (Var "aa"))))))
toForms [(a, b)] = Forms0 $ toForm a b
toForms ((a, b) : xs) = Forms1 (toForm a b) (toForms xs)

toForm :: String -> S.Expr -> Form
toForm s e =
  Form1 $
    Function $ FunctionClauses0 $ toFunctionClause s e

toFunctionClause :: String -> S.Expr -> FunctionClause
toFunctionClause name (ELambda [] e) =
  FunctionClause
    (Atom name)
    (ClauseArgs PatArgumentList0)
    ClauseGuard1
    (ClauseBody $ Exprs0 $ toExpr e)
toFunctionClause name (ELambda xs e) =
  FunctionClause
    (Atom name)
    (ClauseArgs (PatArgumentList1 (listToPatExprs $ map (toPatExpr . eToB) xs)))
    ClauseGuard1
    (ClauseBody $ Exprs0 $ toExpr e)
toFunctionClause name e =
  FunctionClause
    (Atom name)
    (ClauseArgs PatArgumentList0)
    ClauseGuard1
    (ClauseBody $ Exprs0 $ toExpr e)

removeQ :: String -> String
removeQ "" = ""
removeQ s = case last s of
  '\'' -> init s ++ "_Quotes"
  _ -> s

toExpr :: S.Expr -> E.Expr
toExpr (ELChar c) = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic0 c
toExpr (ELInteger i) = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic1 i
toExpr (ELDouble i) = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic2 i
toExpr (ELAtom i) = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic3 $ Atom i
toExpr (ELString i) = Expr13 $ ExprRemote1 $ ExprMax1 $ Atomic4 $ Strings0 i
toExpr EBinary = undefined
toExpr (ETuple []) = Expr13 $ ExprRemote1 $ ExprMax6 Tuple0
toExpr (ETuple ls) = Expr13 $ ExprRemote1 $ ExprMax6 $ Tuple1 (listToExprs $ map toExpr ls)
toExpr (EMapA ls) = Expr10 $ MapExpr0 (listToMapTuple (\x y -> MapField0 $ MapFieldAssoc x y) ls)
toExpr (EMapB ls) = Expr10 $ MapExpr0 (listToMapTuple (\x y -> MapField1 $ MapFieldExact x y) ls)
toExpr (EList ls) = Expr13 $ ExprRemote1 $ ExprMax2 (listToList ls)
toExpr (EPList ls e) = Expr13 $ ExprRemote1 $ ExprMax2 $ listToList1 ls e
toExpr (EVar s) = Expr13 $ ExprRemote1 $ ExprMax0 (Var $ varToUpper $ removeQ s)
toExpr (ELambda [] e) =
  Expr13 $ ExprRemote1 $ ExprMax12 $ FunExpr2 $ FunClauses0 $ FunClause0 PatArgumentList0 ClauseGuard1 (ClauseBody $ Exprs0 (toExpr e))
toExpr (ELambda ls e) =
  Expr13 $ ExprRemote1 $ ExprMax12 $ FunExpr2 $ FunClauses0 $ FunClause0 (PatArgumentList1 (listToPatExprs $ map (toPatExpr . eToB) ls)) ClauseGuard1 (ClauseBody $ Exprs0 (toExpr e))
toExpr (EApp e []) = Expr11 $ FunctionCall (toExprRemote (toExpr e)) ArgumentList0
toExpr (EApp e ls) = Expr11 $ FunctionCall (toExprRemote (toExpr e)) (ArgumentList1 (listToExprs $ map toExpr ls))
toExpr (EModuleCall a b []) = Expr11 $ FunctionCall (toExprRemote2 a b) ArgumentList0
toExpr (EModuleCall a b ls) = Expr11 $ FunctionCall (toExprRemote2 a b) (ArgumentList1 (listToExprs $ map toExpr ls))
toExpr te@(ECase e eas) =
  Expr13 $
    ExprRemote1 $
      ExprMax10 $
        CaseExpr
          (toExpr e)
          (listToCrClause te $ map (\(b, e) -> CrClause (toExpr $ bToE b) ClauseGuard1 (ClauseBody $ Exprs0 $ toExpr e)) eas)
toExpr (EEqualExpr a b) = Expr1 (toExpr a) (toExpr b)
toExpr te@(Receive eas) =
  Expr13 $
    ExprRemote1 $
      ExprMax11 $
        ReceiveExpr0
          (listToCrClause te $ map (\(b, e) -> CrClause (toExpr $ bToE b) ClauseGuard1 (ClauseBody $ Exprs0 $ toExpr e)) eas)
toExpr te@(ReceiveAfter eas (a, b)) =
  Expr13 $
    ExprRemote1 $
      ExprMax11 $
        ReceiveExpr2
          (listToCrClause te $ map (\(b, e) -> CrClause (toExpr $ bToE b) ClauseGuard1 (ClauseBody $ Exprs0 $ toExpr e)) eas)
          (toExpr a)
          (ClauseBody $ Exprs0 $ toExpr b)
toExpr (ELet ls e) = Expr13 $ ExprRemote1 $ ExprMax8 $ listToExprs $ map (\(a, b) -> Expr1 (toExpr a) (toExpr b)) ls ++ [toExpr e]

listToCrClause :: S.Expr -> [CrClause] -> CrClauses
listToCrClause e [] = error (show e)
listToCrClause e [c] = CrClauses0 c
listToCrClause e (x : xs) = CrClauses1 x (listToCrClause e xs)

toExprRemote :: E.Expr -> ExprRemote
toExprRemote (E.Expr13 r) = r
toExprRemote e = ExprRemote1 $ ExprMax7 e

-- toExprRemote e = ExprRemote1 $ ExprMax7 e

toExprRemote2 :: S.Expr -> S.Expr -> ExprRemote
toExprRemote2 a b =
  let Expr13 (ExprRemote1 a') = toExpr a
      Expr13 (ExprRemote1 b') = toExpr b
   in ExprRemote0 a' b'

listToExprs :: [E.Expr] -> Exprs
listToExprs [] = error "never happened"
listToExprs [e] = Exprs0 e
listToExprs (x : xs) = Exprs1 x (listToExprs xs)

listToMapTuple :: (MapKey -> E.Expr -> MapField) -> [(S.Expr, S.Expr)] -> MapTuple
listToMapTuple f [] = MapTuple0
listToMapTuple f xs = MapTuple1 (listToMapFields xs)
  where
    listToMapFields [] = error "never happend"
    listToMapFields [(n, v)] = MapFields0 $ f (MapKey $ toExpr n) (toExpr v)
    listToMapFields ((n, v) : xs) = MapFields1 (f (MapKey $ toExpr n) (toExpr v)) (listToMapFields xs)

listToList :: [S.Expr] -> List
listToList [] = List0
listToList (x : xs) = List1 (toExpr x) (listToTail xs)
  where
    listToTail [] = Tail0
    listToTail (v : vs) = Tail2 (toExpr v) (listToTail vs)

listToList' :: [E.Expr] -> List
listToList' [] = List0
listToList' (x : xs) = List1 x (listToTail xs)
  where
    listToTail [] = Tail0
    listToTail (v : vs) = Tail2 v (listToTail vs)

listToList1 :: [S.Expr] -> S.Expr -> List
listToList1 [] _ = error "never happend" --- ???? [ | ls]
listToList1 (x : xs) e = List1 (toExpr x) (listToTail xs)
  where
    listToTail [] = Tail1 $ toExpr e
    listToTail (v : vs) = Tail2 (toExpr v) (listToTail vs)

listToPatExprs :: [PatExpr] -> PatExprs
listToPatExprs [] = error "never happend"
listToPatExprs [x] = PatExprs0 x
listToPatExprs (x : xs) = PatExprs1 x (listToPatExprs xs)

-- listToArgumentList :: [Exprs]

toPatExpr :: S.Binder -> PatExpr
toPatExpr (BLChar c) = PatExpr8 $ PatExprMax1 $ Atomic0 c
toPatExpr (BLInteger c) = PatExpr8 $ PatExprMax1 $ Atomic1 c
toPatExpr (BLDouble c) = PatExpr8 $ PatExprMax1 $ Atomic2 c
toPatExpr (BLAtom c) = PatExpr8 $ PatExprMax1 $ Atomic3 $ Atom c
toPatExpr (BLString c) = PatExpr8 $ PatExprMax1 $ Atomic4 $ Strings0 c
toPatExpr BBinary = PatExpr8 $ PatExprMax3 undefined
toPatExpr (BList ls) = PatExpr8 $ PatExprMax2 (listToList $ map bToE ls)
toPatExpr (BPList ls e) = PatExpr8 $ PatExprMax2 (listToList1 (map bToE ls) (bToE e))
toPatExpr (BTuple []) = PatExpr8 $ PatExprMax4 Tuple0
toPatExpr (BTuple ls) = PatExpr8 $ PatExprMax4 $ Tuple1 (listToExprs $ map (toExpr . bToE) ls)
toPatExpr (BMapB ls) = PatExpr6 $ MapPatExpr0 (listToMapTuple (\x y -> MapField1 $ MapFieldExact x y) (map (Data.Bifunctor.bimap bToE bToE) ls))
toPatExpr (BVar s) = PatExpr8 $ PatExprMax0 $ Var (varToUpper s)
toPatExpr (BEqualExpr a b) = PatExpr0 (toPatExpr a) (toPatExpr b)
toPatExpr BPatNull = PatExpr8 $ PatExprMax1 $ Atomic3 (Atom "_")

bToE :: S.Binder -> S.Expr
bToE (BLChar c) = ELChar c
bToE (BLInteger c) = ELInteger c
bToE (BLDouble c) = ELDouble c
bToE (BLAtom c) = ELAtom c
bToE (BLString s) = ELString s
bToE BBinary = undefined
bToE (BList ls) = EList (map bToE ls)
bToE (BPList ls b) = EPList (map bToE ls) (bToE b)
bToE (BTuple ls) = ETuple (map bToE ls)
bToE (BMapB ls) = EMapB $ map (Data.Bifunctor.bimap bToE bToE) ls
bToE (BVar s) = EVar $ removeQ s
bToE (BEqualExpr a b) = EEqualExpr (bToE a) (bToE b)
bToE BPatNull = ELAtom "_"

eToB :: S.Expr -> S.Binder
eToB (EVar s) = BVar $ removeQ s
eToB e = error (show e)

varToUpper :: String -> String
varToUpper [] = []
varToUpper (x:xs) = toUpper x : xs
