{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.Hamler.Inline where

import           Data.Map                           as M
import           Data.Text                          (Text, unpack)
import qualified Data.Text.Lazy                     as L
import           Debug.Trace
import           Language.CoreErlang
import Prelude





type BMap = M.Map String (Integer,Expr)

inline :: Ann Module -> Ann Module
inline (Constr (Module a b c d))= let bmap = cFunDefMap d
                       in Constr $  Module a b c (fmap (\(FunDef a ((Constr b))) ->
                                                (FunDef a ((Constr $ iExpr bmap b))))d)
aExpr :: Ann a -> a
aExpr (Constr a) = a
aExpr (Ann a _) = a

ts :: Exprs -> [Expr]
ts (Expr e) =[aExpr e]
ts (Exprs e) = fmap aExpr $ aExpr e

st :: [Expr] -> Exprs
st [] = Exprs $ Constr $ []
st [e] = Expr $ Constr e
st xs =Exprs $ Constr $ fmap Constr xs

st' :: Expr -> Exprs
st' e = Expr $ Constr e

iExpr :: BMap ->  Expr -> Expr
iExpr bmap (EVar v) = EVar v
iExpr bmap (Lit v) = Lit v
iExpr bmap (Fun (FunName (Atom s,n))) = case M.lookup s bmap of
                                          Nothing -> error $ show s ++ "---" ++ show bmap
                                          Just (_,e) -> case n of
                                            0 -> App (Expr $ Constr $ iExpr bmap e) []
                                            _ -> iExpr bmap e

iExpr bmap (App (Expr (Constr e1)) xs ) =
  App (Expr $ Constr $ iExpr bmap e1) (fmap st' $ fmap (iExpr bmap ) $ concat $ fmap ts xs)

iExpr bmap (ModCall ((Expr (Constr e1)),(Expr (Constr e2))) xs) =
  (ModCall ((Expr (Constr $ iExpr bmap e1)),(Expr (Constr $ iExpr bmap e2)))
    ( fmap st' $ fmap (iExpr bmap ) $ concat $ fmap ts  xs))

iExpr bmap (Lam vs xs) = Lam vs ( st $ fmap (iExpr bmap) $ ts xs)

iExpr bmap (Let (vars, expr1) expr2) = Let (vars, st $ fmap (iExpr bmap) $ ts expr1)
                                                 (st $ fmap (iExpr bmap) $ ts expr2)

iExpr bmap (Seq expr1 expr2)  = Seq (st $ fmap (iExpr bmap) $ ts expr1)
                                    (st $ fmap (iExpr bmap) $ ts expr2)

iExpr bmap (Case expr a) = Case (st $ fmap (iExpr bmap) $ ts expr) (fmap (gExpr bmap) a)

iExpr x y = y --error $ show y


gExpr :: BMap ->Ann Alt ->Ann Alt
gExpr bmap (Constr (Alt p g e)) =Constr $ Alt p g (st $ fmap (iExpr bmap) $ ts e)
gExpr bmap (Ann (Alt p g e) _) =Constr $ Alt p g (st $ fmap (iExpr bmap) $ ts e)
gExpr _ x =error $ show x



-- data FunName = FunName (Atom, Integer)
cFunDefMap :: [FunDef] -> M.Map String (Integer,Expr)
cFunDefMap xs= M.fromList $ fmap tfun xs
  where
    tfun :: FunDef -> (String,(Integer,Expr))
    tfun (FunDef (Constr (FunName (Atom s,n))) (Constr expr)) = (s,(n,expr))
    tfun x = error $ show x




