{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Util where

import           Data.Map                         as M
import           Data.Text                        (Text, unpack)
import           Debug.Trace
import           Language.CoreErlang.Syntax       as E
import           Language.PureScript.AST          as A
import qualified Language.PureScript.AST.Literals as L
import           Language.PureScript.Label
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Language.PureScript.Types
import           Prelude

dealpps = decodeStringWithReplacement

aLit2Elit :: L.Literal a -> E.Literal
aLit2Elit (NumericLiteral (Left i))  = LInt i
aLit2Elit (NumericLiteral (Right i)) = LFloat i
aLit2Elit (StringLiteral s)          =
  LString $ decodeStringWithReplacement s
aLit2Elit (CharLiteral c) = LChar c
aLit2Elit (BooleanLiteral True) = LAtom (Atom "true")
aLit2Elit (BooleanLiteral False) = LAtom (Atom "false")
aLit2Elit x  = error "no supply show of literal"

mapsAtom :: Exprs
mapsAtom = Expr $ Constr $ Lit $ LAtom $ Atom "maps"

getAtom :: Exprs
getAtom = Expr $ Constr $ Lit $ LAtom $ Atom "get"

putAtom :: Exprs
putAtom = Expr $ Constr $ Lit $ LAtom $ Atom "put"




opMap :: M.Map Text (E.Exprs,E.Exprs)
opMap = M.fromList [
    ("+",(s2Ee "erlang",s2Ee "+"))
  , ("-",(s2Ee "erlang",s2Ee "-"))
  , ("*",(s2Ee "erlang",s2Ee "*"))
  , ("/",(s2Ee "erlang",s2Ee "/"))
  , (">",(s2Ee "erlang",s2Ee ">"))
  , ("<",(s2Ee "erlang",s2Ee "<"))
  , ("==",(s2Ee "erlang",s2Ee "=="))
  , ("<>",(s2Ee "erlang",s2Ee "append"))
  , ("div",(s2Ee "erlang",s2Ee "div"))
                   ]

expr2exprs = E.Expr . Constr


s2Ee :: String -> E.Exprs
s2Ee s =  E.Expr (Constr $ Lit $ LAtom $ Atom s)


dOp :: A.Expr -> (E.Exprs,E.Exprs)
dOp (Op _ (Qualified _ a)) =
  let Just r =  M.lookup (runOpName a) opMap
  in r


b2Var' :: (Int,A.Binder) -> E.Var
b2Var' (i,b) = b2Vari i b

b2Vari :: Int -> A.Binder -> E.Var
b2Vari i (VarBinder _ _)          = E.Var $ Constr $ ("_" <> show i)
b2Vari i (PositionedBinder _ _ b) = b2Vari i b
b2Vari x     _                    = error $ show x

b2Var ::  A.Binder -> E.Var
b2Var  (VarBinder _ i)          = E.Var $ Constr $ (unpack $ runIdent i)
b2Var  (PositionedBinder _ _ b) = b2Var b
b2Var x                         = error $ show x



binder2Indent :: Binder -> Ident
binder2Indent (VarBinder _ i)          = i
binder2Indent (PositionedBinder _ _ b) = binder2Indent b
binder2Indent x                        = error $ show x

bin2pat :: A.Binder -> E.Pat
bin2pat NullBinder               = PLit $  LNil  --- erlang -- nullBinder
bin2pat (LiteralBinder _ e)      = PLit  $ aLit2Elit e
bin2pat (VarBinder _ i)          = PVar $ E.Var $ Constr $ unpack $ runIdent i
bin2pat (PositionedBinder _ _ b) = bin2pat b
bin2pat  x                       = error $ show x


-- decodeStringWithReplacement
type2labels :: Type a -> [String] -> [String]
type2labels (TypeApp _ a b) xs  = type2labels b xs
type2labels (RCons _ l _ b) xs=
  type2labels b ( (decodeStringWithReplacement $ runLabel $ l) : xs)
type2labels (REmpty _) xs =xs

type2labels' = reverse . (flip type2labels [])









