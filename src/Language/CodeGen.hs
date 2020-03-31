{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.CodeGen where

import           Data.Map                         as M
import           Data.Text                        (Text, unpack)

import           Control.Monad.State.Strict
import           Data.Set                         as S

import           Debug.Trace
import           Language.CoreErlang.Syntax       as E
import           Language.PureScript.AST          as A
import qualified Language.PureScript.AST.Literals as L
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Language.Util
import           Prelude


aM2Em :: A.Module -> E.Module
aM2Em (A.Module sp com modename decls maybeDecl) =
  E.Module (Atom "main") (concat $ fmap exportDef decls) [] (Prelude.concat $ Prelude.map (aD2Ed' decls) decls)


exportDef :: A.Declaration ->  [FunName]
exportDef  (ValueDeclaration
        (ValueDeclarationData _ i _ b g)
       ) = case g of
             [r] ->  [FunName (Atom $  (unpack $ runIdent i) , toInteger $ length b )]
exportDef (DataDeclaration _ _ _ _ ds) =fmap  (\p -> FunName $ getsome p ) ds
  where getsome (DataConstructorDeclaration _ p dfs) = (Atom $ unpack $ runProperName p,
                                toInteger $ length  (type2labels' $ snd $ head dfs))
exportDef _ = undefined

data GState = GState { globalVar :: M.Map Ident Integer -- k=ident, v= the number of args
                     , localVar  :: M.Map Ident Int
                     }  deriving (Eq,Ord,Show)

initGState :: [Declaration] -> [Binder] -> GState
initGState ds binders= GState {
    globalVar = M.fromList $ concat $ fmap t ds
  , localVar = M.fromList $ flip zip  [0..] $ fmap binder2Indent binders
                       }
  where t  (ValueDeclaration (ValueDeclarationData _ i _ bs g) ) = case g of
             [r] ->   [(i, fromIntegral $ length bs)]
        t (DataDeclaration _ _ _ _ ds)= fmap getsome ds
          where getsome (DataConstructorDeclaration _ p dfs) = ( Ident $ runProperName p, toInteger $ length  (type2labels' $ snd $ head dfs))
        t _ = undefined

aD2Ed' :: [Declaration] -> A.Declaration -> [FunDef]
aD2Ed' decls (ValueDeclaration
        (ValueDeclarationData _ i _ bs g)
       ) = case g of
             [r] -> [ FunDef (Constr $ FunName (Atom $ unpack $ runIdent i,  toInteger $ length bs) )
                 (Constr $ Lambda (fmap b2Var' $ zip [0..] bs ) (Expr $ Constr $ aGE2EE' r $ initGState decls bs )) ]
aD2Ed' _  d@(DataDeclaration _ _ _ _ ds) = fmap deal ds    -- error $ show $  [ show ds]
  where deal (DataConstructorDeclaration _ p dfs)  =
          let name = unpack $ runProperName p
          in FunDef (Constr $ FunName (Atom $ unpack $ runProperName p,  toInteger $ length $ type2labels' $ snd $ head dfs) )
             (Constr $ Lambda (fmap (\(i,_) ->E.Var $ Constr $ "_" <> show i)  $ zip [0..] (type2labels' $ snd $ head dfs) )
               ( Prelude.foldl foldFun initState $ (zip [0..] ((type2labels' (snd $ head dfs)))))) --tuple
        initState :: Exprs
        initState = Expr $ Constr $ EMap (Map [])
        foldFun b (i,str) = Expr $ Constr $ ModCall (mapsAtom,putAtom) [ Expr $ Constr $ Lit $ LAtom $ Atom  str
                                                                       ,(Expr $ Constr $ E.EVar $ E.Var  $ Constr ("_" <> show i)),b] -- k v m

aD2Ed' _ x = error $ show x

-- | convert purescript AST expr into erlang expr
aE2EE' :: A.Expr -> GState -> E.Expr
aE2EE' (A.Literal _ l)    gs       = Lit $ aLit2Elit l
aE2EE' (UnaryMinus _ e )     gs    = undefined -- 暂时用不到
aE2EE' (BinaryNoParens e1 e2 e3) gs = ModCall (dOp e1)
                                  [Expr $ Constr $ aE2EE' e2 gs
                                  , Expr $ Constr $ aE2EE' e3 gs
                                  ]
aE2EE' (A.Parens e)       gs    = aE2EE' e gs
aE2EE' (A.Accessor ps e)   gs        = ModCall (mapsAtom,getAtom)  [(E.Expr $ Constr $ Lit $ LAtom $ Atom $ dealpps ps)
                                                                   ,E.Expr $ Constr $ E.App (E.Expr $ Constr $ aE2EE' e gs) []]
aE2EE' (A.ObjectUpdate e xs)  gs    = undefined --Prelude.foldl foldFun1 initS xs
  where initS =  E.App (E.Expr $ Constr $ aE2EE' e gs) []
        foldFun1 b (ps,expr) = ModCall (mapsAtom,putAtom) [(E.Expr $ Constr $ Lit $ LAtom $ Atom $ dealpps ps)
                                                         , E.Expr $ Constr $ aE2EE' expr gs
                                                         ,E.Expr $ Constr $ b]

aE2EE' (A.ObjectUpdateNested  e pte)  gs          = error $ show pte

-- aE2EE (A.Abs (VarBinder _ b) e)           = Lambda [E.Var $ Constr $ unpack $ runIdent b] (Expr $ Constr $ aE2EE e)
aE2EE' app@(A.App e1 e2) gs = myfun gs app [] -- E.App (expr2exprs $ aE2EE' ( e1) gs) [ expr2exprs $ aE2EE' (e2) gs]
  where myfun gs left xs = case left of
          ((A.App l r)) -> myfun gs  l (aE2EE' r gs:xs)
          (PositionedValue _ _ (A.App l r)) -> myfun gs  l (aE2EE' r gs:xs)
          o ->E.App ( expr2exprs $ aE2EE' o gs) (fmap expr2exprs  xs)
aE2EE' (A.Unused e) gs= undefined
aE2EE' (A.Var _ (Qualified _ v)) GState{..} = nres
  where nres = case M.lookup v globalVar of
          Just args -> E.Fun $ E.FunName (Atom $ unpack $ runIdent v, args)
          Nothing -> case M.lookup v localVar of
                       Just i  -> E.EVar $ E.Var $  Constr $ "_" <> show i
                       Nothing -> error "there isn't the var!"
-- aE2EE (Op _ _ ) = --
aE2EE' (IfThenElse e1 e2 e3) gs= let atom = expr2exprs $ Lit $ LAtom $ Atom "IfThenElse"
                                     literal = LAtom $ Atom "IfThenElse"
                                     e1' = aE2EE' e1 gs
                                     e2' = aE2EE' e2 gs
                                     e3' = aE2EE' e3 gs
                                 in  E.Case atom [
  Constr $ Alt (Pat $ Constr $ PLit  literal) (Guard $ Expr $ Constr $ e1') (Expr $ Constr e2') ,
  Constr $ Alt (Pat $ Constr $ PLit  literal) (Guard $ Expr $ Constr $ Lit $ LAtom $ Atom "true") (Expr $ Constr e3')]
aE2EE' (A.Constructor _ (Qualified _ (ProperName v))) GState{..}= case M.lookup (Ident v) globalVar of
                                               Just args -> E.Fun $ E.FunName (Atom $ unpack v, args)
                                               Nothing -> error "this isn't true"
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








