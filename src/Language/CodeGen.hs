{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.CodeGen where

import           Data.Map                         as M
import           Data.Text                        (Text, unpack)

import           Control.Monad.State.Strict
import           Data.Set                         as S

import           Language.CoreErlang.Syntax       as E
import           Language.PureScript.AST          as A
import qualified Language.PureScript.AST.Literals as L
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Language.Util
import           Prelude



aM2Em :: A.Module -> E.Module
aM2Em (A.Module sp com modename decls maybeDecl) =
  E.Module (Atom "main") (fmap exportDef decls) [] (Prelude.map (aD2Ed' decls) decls)

aD2Ed :: A.Declaration -> FunDef
aD2Ed  (ValueDeclaration
        (ValueDeclarationData _ i _ b g)
       ) = case g of
             [r] ->  FunDef (Constr $ FunName (Atom $ unpack $ runIdent i,  toInteger $ length b) )
                 (Constr $ Lambda (fmap b2Var b ) (Expr $ Constr $ aGE2EE r ))
aD2Ed _ = undefined


exportDef :: A.Declaration ->  FunName
exportDef  (ValueDeclaration
        (ValueDeclarationData _ i _ b g)
       ) = case g of
             [r] ->  FunName (Atom $  (unpack $ runIdent i) , toInteger $ length b )
exportDef _ = undefined

data GState = GState { globalVar :: S.Set Ident
                     , localVar  :: M.Map Ident Int
                     }  deriving (Eq,Ord,Show)



initGState :: [Declaration] -> [Binder] -> GState
initGState ds binders= GState {
    globalVar = S.fromList $ fmap t ds
  , localVar = M.fromList $ flip zip  [0..] $ fmap binder2Indent binders
                       }
  where t  (ValueDeclaration (ValueDeclarationData _ i _ b g) ) = case g of
             [r] ->   i
        t _ = undefined

aD2Ed' :: [Declaration] -> A.Declaration -> FunDef
aD2Ed' decls (ValueDeclaration
        (ValueDeclarationData _ i _ bs g)
       ) = case g of
             [r] ->  FunDef (Constr $ FunName (Atom $ unpack $ runIdent i,  toInteger $ length bs) )
                 (Constr $ Lambda (fmap b2Var' $ zip [0..] bs ) (Expr $ Constr $ aGE2EE' r $ initGState decls bs ))
aD2Ed' _ _ = undefined





-- aD2Ed' :: A.Declaration  -> GState-> FunDef
-- aD2Ed'  (ValueDeclaration
--         (ValueDeclarationData _ i _ b g)
--        ) GState{..}= case g of
--              [r] ->  FunDef (Constr $ FunName (Atom $ unpack $ runIdent i,  toInteger $ length b) )
--                  (Constr $ Lambda (fmap b2Var b ) (Expr $ Constr $ undefined r ))
-- aD2Ed' _  _ = undefined

aE2EE' :: A.Expr -> GState -> E.Expr

aE2EE' (A.Literal _ l)    gs       = Lit $ aLit2Elit l

aE2EE' (UnaryMinus _ e )     gs    = undefined -- 暂时用不到

aE2EE' (BinaryNoParens e1 e2 e3) gs = ModCall (dOp e1)
                                  [Expr $ Constr $ aE2EE' e2 gs
                                  , Expr $ Constr $ aE2EE' e3 gs
                                  ]
aE2EE' (A.Parens e)       gs    = aE2EE' e gs

aE2EE' (A.Accessor ps e)   gs        = undefined


aE2EE' (A.ObjectUpdate e xs)  gs         = undefined

aE2EE' (A.ObjectUpdateNested  e pte)  gs          = undefined

-- aE2EE (A.Abs (VarBinder _ b) e)           = Lambda [E.Var $ Constr $ unpack $ runIdent b] (Expr $ Constr $ aE2EE e)

aE2EE' (A.App e1 e2) gs = E.App (expr2exprs $ aE2EE' e1 gs) [ expr2exprs $ aE2EE' e2 gs]

aE2EE' (A.Unused e) gs= undefined

aE2EE' (A.Var _ (Qualified _ v)) GState{..} = E.EVar $ E.Var $  Constr $ res
                                            where res = if v `S.member` globalVar
                                                        then  unpack $ runIdent v
                                                        else case M.lookup v localVar of
                                                               Nothing -> error "there isn't the var!"
                                                               Just i -> "_" <> show i


-- aE2EE (Op _ _ ) = --

aE2EE' (IfThenElse e1 e2 e3) gs= let atom = expr2exprs $ Lit $ LAtom $ Atom "IfThenElse"
                                     literal = LAtom $ Atom "IfThenElse"
                                     e1' = aE2EE' e1 gs
                                     e2' = aE2EE' e2 gs
                                     e3' = aE2EE' e3 gs
                                 in  E.Case atom [
  Constr $ Alt (Pat $ Constr $ PLit  literal) (Guard $ Expr $ Constr $ e1') (Expr $ Constr e2') ,
  Constr $ Alt (Pat $ Constr $ PLit  literal) (Guard $ Expr $ Constr $ Lit $ LAtom $ Atom "true") (Expr $ Constr e2')
                                            ]
aE2EE' (A.Constructor _ b) gs= undefined
aE2EE' (A.Case es calts) g0= E.Case (E.Exprs $ Constr $  fmap (\e ->  Constr $ aE2EE' e g0) es)
                           ( do
                           (CaseAlternative bids gs) <- calts
                           let pats = E.Pats $ Constr $ fmap (\b-> (Constr $ bin2pat b)) bids
                           GuardedExpr gus e <- gs
                           let gurad = E.Guard $ E.Exprs $ Constr
                                 $ ( if Prelude.null gus
                                     then [Constr $ Lit $ LAtom $ Atom "true"]
                                          ---     PatternGuard need to do!
                                     else fmap (\(ConditionGuard cg) ->  Constr $ aE2EE' cg g0) gus
                                   )
                               exprs = E.Expr $ Constr $ aE2EE' e g0
                           return $ Constr $ (Alt pats gurad exprs)
                           )
-- aE2EE (A.Let w ds e) = undefined



aE2EE' (PositionedValue _ _ e)  gs= aE2EE' e gs

aE2EE' e gs = error $ show e


aGE2EE' :: A.GuardedExpr -> GState -> E.Expr
aGE2EE' (GuardedExpr gs e) g0= aE2EE' e g0



-- aG2EE :: A.Guard -> E.Expr
-- aG2EE (ConditionGuard e ) = aE2EE e
-- aG2EE (PatternGuard b e ) = aE2EE e

aGE2EE :: A.GuardedExpr -> E.Expr
aGE2EE (GuardedExpr gs e) = aE2EE e

aE2EE :: A.Expr -> E.Expr
aE2EE (A.Literal _ l)           = Lit $ aLit2Elit l

aE2EE (UnaryMinus _ e )         = undefined -- 暂时用不到

aE2EE (BinaryNoParens e1 e2 e3) = ModCall (dOp e1)
                                  [Expr $ Constr $ aE2EE e2
                                  , Expr $ Constr $ aE2EE e3
                                  ]
aE2EE (A.Parens e)           = aE2EE e

aE2EE (A.Accessor ps e)           = undefined


aE2EE (A.ObjectUpdate e xs)           = undefined

aE2EE (A.ObjectUpdateNested  e pte)           = undefined

-- aE2EE (A.Abs (VarBinder _ b) e)           = Lambda [E.Var $ Constr $ unpack $ runIdent b] (Expr $ Constr $ aE2EE e)

aE2EE (A.App e1 e2)  = E.App (expr2exprs $ aE2EE e1) [ expr2exprs $ aE2EE e2]

aE2EE (A.Unused e) = undefined

aE2EE (A.Var _ (Qualified _ v)) = E.EVar $ E.Var $  Constr $ unpack $ runIdent v

-- aE2EE (Op _ _ ) = --

aE2EE (IfThenElse e1 e2 e3) = let atom = expr2exprs $ Lit $ LAtom $ Atom "IfThenElse"
                                  literal = LAtom $ Atom "IfThenElse"
                                  e1' = aE2EE e1
                                  e2' = aE2EE e2
                                  e3' = aE2EE e3
                              in  E.Case atom [
  Constr $ Alt (Pat $ Constr $ PLit  literal) (Guard $ Expr $ Constr $ e1') (Expr $ Constr e2') ,
  Constr $ Alt (Pat $ Constr $ PLit  literal) (Guard $ Expr $ Constr $ Lit $ LAtom $ Atom "true") (Expr $ Constr e2')
                                            ]
aE2EE (A.Constructor _ b) = undefined
aE2EE (A.Case es calts) = E.Case (E.Exprs $ Constr $  fmap (\e ->  Constr $ aE2EE e) es)
                           ( do
                           (CaseAlternative bids gs) <- calts
                           let pats = E.Pats $ Constr $ fmap (\b-> (Constr $ bin2pat b)) bids
                           GuardedExpr gus e <- gs
                           let gurad = E.Guard $ E.Exprs $ Constr
                                 $ ( if Prelude.null gus
                                     then [Constr $ Lit $ LAtom $ Atom "true"]
                                          ---     PatternGuard need to do!
                                     else fmap (\(ConditionGuard cg) ->  Constr $ aE2EE cg) gus
                                   )
                               exprs = E.Expr $ Constr $ aE2EE e
                           return $ Constr $ (Alt pats gurad exprs)
                           )
-- aE2EE (A.Let w ds e) = undefined



aE2EE (PositionedValue _ _ e)  = aE2EE e




aE2EE x  = error $ show x







