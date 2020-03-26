module Language.CodeGen where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Language.CoreErlang.Syntax
import qualified Language.PureScript.CoreFn   as C
import           Language.PureScript.PSString (decodeStringWithReplacement)
import           Prelude

type Result = Except String Expr

corexpr2expr :: C.Expr C.Ann -> Result
corexpr2expr expr = undefined



corelit2expr :: C.Literal (C.Expr C.Ann) -> Result
corelit2expr (C.NumericLiteral (Left i)) = return $  Lit $ LInt i
corelit2expr (C.StringLiteral ppstring) = return $ Lit $ LString $ decodeStringWithReplacement ppstring
corelit2expr (C.CharLiteral c) = return $ Lit $ LChar c
corelit2expr (C.BooleanLiteral True) = return $ Lit $ LAtom $ Atom "true"
corelit2expr (C.BooleanLiteral False) = return $ Lit $ LAtom $ Atom "false"
corelit2expr (C.ArrayLiteral xs) = do
  res <- mapM corexpr2expr xs
  -- 完全没懂是什么意思。。。。。。。
  return $ List $ L $ [Exprs $ Ann undefined undefined]
corelit2expr (C.ObjectLiteral xs) = do
  res <- forM xs $ \(k,v) -> do
    rk <_ corexpr2expr k
    rv <- corexpr2expr v
    return (rk,rv)
  return $ EMap $ Map $ undefined res
---- 看不懂 CoreErlang 中的 Exprs 表达什么意思











