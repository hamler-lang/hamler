{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.Fixity
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Fixity information to give the parser so that infix operators can
-- be parsed properly.
--
-----------------------------------------------------------------------------
module Hamler.AST.Fixity
    (
    -- * Fixity representation
      Fixity(..)
    -- | The following three functions all create lists of
    --   fixities from textual representations of operators.
    --   The intended usage is e.g.
    --
    -- > fixs = infixr_ 0  ["$","$!","`seq`"]
    --
    --   Note that the operators are expected as you would
    --   write them infix, i.e. with ` characters surrounding
    --   /varid/ operators, and /varsym/ operators written as is.
    , infix_, infixl_, infixr_
    -- ** Collections of fixities
    , preludeFixities, baseFixities

    -- * Applying fixities to an AST
    , AppFixity(..)
    ) where

import Hamler.AST.Syntax
import Hamler.AST.SrcLoc

import Control.Monad (when, (<=<), liftM, liftM2, liftM3, liftM4)
import qualified Control.Monad.Fail as Fail
import Data.Traversable (mapM)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Data.Data hiding (Fixity)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$))
#endif
import Prelude hiding (mapM)

-- | Operator fixities are represented by their associativity
--   (left, right or none) and their precedence (0-9).
data Fixity = Fixity (Assoc ()) Int (QName ())
  deriving (Eq,Ord,Show,Typeable,Data)

-- | All AST elements that may include expressions which in turn may
--   need fixity tweaking will be instances of this class.
class AppFixity ast where
  -- | Tweak any expressions in the element to account for the
  --   fixities given. Assumes that all operator expressions are
  --   fully left associative chains to begin with.
  applyFixities :: Fail.MonadFail m => [Fixity]  -- ^ The fixities to account for.
                    -> ast SrcSpanInfo           -- ^ The element to tweak.
                    -> m (ast SrcSpanInfo)       -- ^ The same element, but with operator expressions updated, or a failure.

assocNone, assocLeft, assocRight :: Assoc ()
assocNone = AssocNone ()
assocLeft = AssocLeft ()
assocRight = AssocRight ()

instance AppFixity Exp where
  applyFixities fixs' = infFix fixs' <=< leafFix fixs'
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (InfixApp l2 a op2 z) = do
              e <- infFix fixs a
              let fixup (a1,p1) (a2,p2) y pre = do
                      when (p1 == p2 && (a1 /= a2 || a1 == assocNone)) -- Ambiguous infix expression!
                           $ fail "Ambiguous infix expression"
                      if p1 > p2 || p1 == p2 && (a1 == assocLeft || a2 == assocNone) -- Already right order
                       then return $ InfixApp l2 e op2 z
                       else liftM pre (infFix fixs $ InfixApp (ann y <++> ann z) y op2 z)
              case e of
               InfixApp _ x op1 y -> fixup (askFixity fixs op1) (askFixity fixs op2) y (InfixApp l2 x op1)
               NegApp   _       y -> fixup prefixMinusFixity    (askFixity fixs op2) y (NegApp l2)
               _  -> return $ InfixApp l2 e op2 z

          infFix _ e = return e

--ambOps l = ParseFailed (getPointLoc l) $ "Ambiguous infix expression"

instance AppFixity Pat where
  applyFixities fixs' = infFix fixs' <=< leafFixP fixs'
    where -- This is the real meat case. We can assume a left-associative list to begin with.
          infFix fixs (PInfixApp l2 a op2 z) = do
              p <- infFix fixs a
              let fixup (a1,p1) (a2,p2) y pre = do
                      when (p1 == p2 && (a1 /= a2 || a1 == assocNone )) -- Ambiguous infix expression!
                           $ fail "Ambiguous infix expression"
                      if p1 > p2 || p1 == p2 && (a1 == assocLeft || a2 == assocNone) -- Already right order
                       then return $ PInfixApp l2 p op2 z
                       else liftM pre (infFix fixs $ PInfixApp (ann y <++> ann z) y op2 z)
              case p of
               PInfixApp _ x op1 y -> fixup (askFixityP fixs op1) (askFixityP fixs op2) y (PInfixApp l2 x op1)
               _  -> return $ PInfixApp l2 p op2 z

          infFix _ p = return p

-- Internal: lookup associativity and precedence of an operator
askFixity :: [Fixity] -> QOp l -> (Assoc (), Int)
askFixity xs k = askFix xs (f (() <$ k))
    where
        f (QVarOp _ x) = g x
        f (QConOp _ x) = g x

        g (Special _ (Cons _)) = UnQual () (Symbol () ":")
        g x                  = x

-- Same using patterns
askFixityP :: [Fixity] -> QName l -> (Assoc (), Int)
askFixityP xs qn = askFix xs (g (() <$ qn))
    where
        g (Special _ (Cons _)) = UnQual () (Symbol () ":")
        g x                  = x

askFix :: [Fixity] -> QName l -> (Assoc (), Int)
askFix xs = \k -> lookupWithDefault (assocLeft, 9) (() <$ k) mp
    where
        lookupWithDefault def k mp' = fromMaybe def $ lookup k mp'

        mp = [(x,(a,p)) | Fixity a p x <- xs]



-- | Built-in fixity for prefix minus
prefixMinusFixity :: (Assoc (), Int)
prefixMinusFixity = (AssocLeft (), 6)

-- | All fixities defined in the Prelude.
preludeFixities :: [Fixity]
preludeFixities = concat
    [infixr_ 9  ["."]
    ,infixl_ 9  ["!!"]
    ,infixr_ 8  ["^","^^","**"]
    ,infixl_ 7  ["*","/","`quot`","`rem`","`div`","`mod`"]
    ,infixl_ 6  ["+","-"]
    ,infixr_ 5  [":","++"]
    ,infix_  4  ["==","/=","<","<=",">=",">","`elem`","`notElem`"]
    ,infixl_ 4  ["<$>","<$","<*>","<*","*>"]
    ,infixr_ 3  ["&&"]
    ,infixr_ 2  ["||"]
    ,infixl_ 1  [">>",">>="]
    ,infixr_ 1  ["=<<"]
    ,infixr_ 0  ["$","$!","`seq`"]
    ]

-- | All fixities defined in the base package.
--
--   Note that the @+++@ operator appears in both Control.Arrows and
--   Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
--   this list is that of Control.Arrows.
baseFixities :: [Fixity]
baseFixities = preludeFixities ++ concat
    [infixl_ 9 ["!","//","!:"]
    ,infixr_ 9 ["`Compose`"]
    ,infixl_ 8 ["`shift`","`rotate`","`shiftL`","`shiftR`","`rotateL`","`rotateR`"]
    ,infixl_ 7 [".&.","%"]
    ,infixr_ 6 ["<>"]
    ,infixl_ 6 ["`xor`"]
    ,infix_  6 [":+"]
    ,infixl_ 5 [".|."]
    ,infixr_ 5 ["+:+","<++","<+>","<|"] -- fixity conflict for +++ between ReadP and Arrow
    ,infix_  5 ["\\\\"]
    ,infixl_ 4 ["<**>","$>","<$","<$!>"]
    ,infix_  4 ["`elemP`","`notElemP`",":~:",":~~:"]
    ,infixl_ 3 ["<|>"]
    ,infixr_ 3 ["&&&","***"]
    ,infixr_ 2 ["+++","|||"]
    ,infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    ,infixl_ 1 ["&"]
    ,infixl_ 0 ["`on`"]
    ,infixr_ 0 ["`par`","`pseq`"]
    ]

infixr_, infixl_, infix_ :: Int -> [String] -> [Fixity]
infixr_ = fixity  assocRight
infixl_ = fixity  assocLeft
infix_  = fixity  assocNone

-- Internal: help function for the above definitions.
fixity :: Assoc () -> Int -> [String] -> [Fixity]
fixity a p = map (Fixity a p . op)
    where
        op ('`':xs) = UnQual () $ Ident () $ init xs
        op xs = UnQual () $ Symbol () xs








-------------------------------------------------------------------
-- Boilerplate - yuck!! Everything below here is internal stuff

instance AppFixity Module where
    applyFixities fixs (Module l mmh prs imp decls) =
        liftM (Module l mmh prs imp) $ appFixDecls mmn fixs decls
      where mmn = getMmn mmh
            getMmn (Just (ModuleHead _ n _ _)) = Just n
            getMmn _ = Nothing

instance AppFixity Decl where
    applyFixities fixs decl = case decl of
        ClassDecl l ctxt dh deps cdecls   -> liftM (ClassDecl l ctxt dh deps) $ mapM (mapM fix) cdecls
        InstDecl  l olp ih idecls         -> liftM (InstDecl  l olp ih)  $ mapM (mapM fix) idecls
        FunBind l matches       -> liftM (FunBind l) $ mapM fix matches
        PatBind l p rhs bs      ->
         let extraFix x = applyFixities (fixs ++ maybe [] getBindFixities bs) x
          in liftM3 (PatBind l) (extraFix p) (extraFix rhs) (mapM extraFix bs)
        AnnPragma l ann'         -> liftM (AnnPragma l) $ fix ann'
        PatSyn l p1 p2 dir -> liftM (PatSyn l p1 p2) (fix dir)
        _                       -> return decl
      where fix x = applyFixities fixs x

instance AppFixity PatternSynDirection where
  applyFixities fixs dir = case dir of
    ExplicitBidirectional l ds -> liftM (ExplicitBidirectional l) (mapM fix ds)
    _ -> return dir
    where fix x = applyFixities fixs x

appFixDecls :: Fail.MonadFail m => Maybe (ModuleName SrcSpanInfo) -> [Fixity] -> [Decl SrcSpanInfo] -> m [Decl SrcSpanInfo]
appFixDecls mmdl fixs decls =
    let extraFixs = getFixities mmdl decls
     in mapM (applyFixities (fixs++extraFixs)) decls

getFixities :: Maybe (ModuleName l) -> [Decl l] -> [Fixity]
getFixities mmdl = concatMap (getFixity mmdl)

getFixity :: Maybe (ModuleName l) -> Decl l -> [Fixity]
getFixity mmdl d =
  case d of
    InfixDecl _ a mp ops  -> let p = fromMaybe 9 mp
                              in map (Fixity (scrub a) p) (concatMap g (map scrub ops))
    ClassDecl _ _ _ _ cds -> maybe [] (concatMap getClassFixity) cds
    _ -> []
  where g (VarOp _ x) = f x
        g (ConOp _ x) = f x
        f x = case mmdl of
              Nothing -> [UnQual () x]
              Just m  -> [Qual () (scrub m) x, UnQual () x]
        getClassFixity (ClsDecl _ cd) = getFixity mmdl cd
        getClassFixity _              = []

scrub :: Functor f => f a -> f ()
scrub f = () <$ f

getBindFixities :: Binds l -> [Fixity]
getBindFixities bs = case bs of
                        BDecls _ ds -> getFixities Nothing ds
                        _           -> []

instance AppFixity Annotation where
    applyFixities fixs ann' = case ann' of
        Ann     l n e   -> liftM (Ann l n) $ fix e
        TypeAnn l n e   -> liftM (TypeAnn l n) $ fix e
        ModuleAnn l e   -> liftM (ModuleAnn l) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity ClassDecl where
    applyFixities fixs (ClsDecl l decl) = liftM (ClsDecl l) $ applyFixities fixs decl
    applyFixities _ cdecl = return cdecl

instance AppFixity InstDecl where
    applyFixities fixs (InsDecl l decl) = liftM (InsDecl l) $ applyFixities fixs decl
    applyFixities _ idecl = return idecl

instance AppFixity Match where
    applyFixities fixs match = case match of
        Match l n ps rhs bs -> liftM3 (Match l n) (mapM (fix bs) ps) (fix bs rhs) (mapM (fix bs) bs)
        InfixMatch l a n ps rhs bs -> liftM4 (flip (InfixMatch l) n) (fix bs a) (mapM (fix bs) ps) (fix bs rhs) (mapM (fix bs) bs)
      where fix bs x = applyFixities fixs' x
             where fixs' = fixs ++ maybe [] getBindFixities bs

instance AppFixity Rhs where
    applyFixities fixs rhs = case rhs of
        UnGuardedRhs l e      -> liftM (UnGuardedRhs l) $ fix e
        GuardedRhss l grhss   -> liftM (GuardedRhss l) $ mapM fix grhss
      where fix x = applyFixities fixs x

instance AppFixity GuardedRhs where
    applyFixities fixs (GuardedRhs l stmts e) = liftM2 (GuardedRhs l) (mapM fix stmts) $ fix e
      where fix x = applyFixities fixs x

instance AppFixity PatField where
    applyFixities fixs (PFieldPat l n p) = liftM (PFieldPat l n) $ applyFixities fixs p
    applyFixities _ pf = return pf

instance AppFixity Stmt where
    applyFixities fixs stmt = case stmt of
        Generator l p e       -> liftM2 (Generator l) (fix p) (fix e)
        Qualifier l e         -> liftM (Qualifier l) $ fix e
        LetStmt l bs          -> liftM (LetStmt l) $ fix bs    -- special behavior
        RecStmt l stmts       -> liftM (RecStmt l) $ mapM fix stmts
      where fix x = applyFixities fixs x

instance AppFixity Binds where
    applyFixities fixs bs = case bs of
        BDecls l decls        -> liftM (BDecls l) $ appFixDecls Nothing fixs decls  -- special behavior
        IPBinds l ips         -> liftM (IPBinds l) $ mapM fix ips
      where fix x = applyFixities fixs x

instance AppFixity IPBind where
    applyFixities fixs (IPBind l n e) = liftM (IPBind l n) $ applyFixities fixs e

instance AppFixity FieldUpdate where
    applyFixities fixs (FieldUpdate l n e) = liftM (FieldUpdate l n) $ applyFixities fixs e
    applyFixities _ fup = return fup

instance AppFixity Alt where
    applyFixities fixs (Alt l p galts bs) = liftM3 (Alt l) (fix p) (fix galts) (mapM fix bs)
      where fix x = applyFixities fixs x

instance AppFixity QualStmt where
    applyFixities fixs qstmt = case qstmt of
        QualStmt     l s      -> liftM (QualStmt l) $ fix s
      where fix x = applyFixities fixs x

-- the boring boilerplate stuff for expressions too
-- Recursively fixes the "leaves" of the infix chains,
-- without yet touching the chain itself. We assume all chains are
-- left-associate to begin with.
leafFix :: Fail.MonadFail m => [Fixity] -> Exp SrcSpanInfo -> m (Exp SrcSpanInfo)
leafFix fixs e' = case e' of
    InfixApp l e1 op e2       -> liftM2 (flip (InfixApp l) op) (leafFix fixs e1) (fix e2)
    App l e1 e2               -> liftM2 (App l) (fix e1) (fix e2)
    NegApp l e                -> liftM (NegApp l) $ fix e
    Lambda l pats e           -> liftM2 (Lambda l) (mapM fix pats) $ fix e
    Let l bs e                ->
        let extraFix x = applyFixities (fixs ++ getBindFixities bs) x
         in liftM2 (Let l) (extraFix bs) $ extraFix e
    If l e a b                -> liftM3 (If l) (fix e) (fix a) (fix b)
    MultiIf l alts            -> liftM (MultiIf l) (mapM fix alts)
    Case l e alts             -> liftM2 (Case l) (fix e) $ mapM fix alts
    Do l stmts                -> liftM (Do l) $ mapM fix stmts
    MDo l stmts               -> liftM (MDo l) $ mapM fix stmts
    Tuple l bx exps           -> liftM (Tuple l bx) $ mapM fix exps
    List l exps               -> liftM (List l) $ mapM fix  exps
    Paren l e                 -> liftM (Paren l) $ fix e
    LeftSection l e op        -> liftM (flip (LeftSection l) op) (fix e)
    RightSection l op e       -> liftM (RightSection l op) $ fix e
    RecConstr l n fups        -> liftM (RecConstr l n) $ mapM fix fups
    RecUpdate l e fups        -> liftM2 (RecUpdate l) (fix e) $ mapM fix fups
    EnumFrom l e              -> liftM (EnumFrom l) $ fix e
    EnumFromTo l e1 e2        -> liftM2 (EnumFromTo l) (fix e1) (fix e2)
    EnumFromThen l e1 e2      -> liftM2 (EnumFromThen l) (fix e1) (fix e2)
    EnumFromThenTo l e1 e2 e3 -> liftM3 (EnumFromThenTo l) (fix e1) (fix e2) (fix e3)
    ListComp l e quals        -> liftM2 (ListComp l) (fix e) $ mapM fix quals
    ParComp  l e qualss       -> liftM2 (ParComp l) (fix e) $ mapM (mapM fix) qualss
    ExpTypeSig l e t          -> liftM (flip (ExpTypeSig l) t) (fix e)
    CorePragma l s e          -> liftM (CorePragma l s) (fix e)
    SCCPragma l s e           -> liftM (SCCPragma l s) (fix e)
    GenPragma l s ab cd e     -> liftM (GenPragma l s ab cd) (fix e)
    _                         -> return e'
  where
    fix x = applyFixities fixs x

leafFixP :: Fail.MonadFail m => [Fixity] -> Pat SrcSpanInfo -> m (Pat SrcSpanInfo)
leafFixP fixs p' = case p' of
        PInfixApp l p1 op p2    -> liftM2 (flip (PInfixApp l) op) (leafFixP fixs p1) (fix p2)
        PApp l n ps             -> liftM (PApp l n) $ mapM fix ps
        PTuple l bx ps          -> liftM (PTuple l bx) $ mapM fix ps
        PList l ps              -> liftM (PList l) $ mapM fix ps
        PParen l p              -> liftM (PParen l) $ fix p
        PRec l n pfs            -> liftM (PRec l n) $ mapM fix pfs
        PAsPat l n p            -> liftM (PAsPat l n) $ fix p
        PIrrPat l p             -> liftM (PIrrPat l) $ fix p
        PatTypeSig l p t        -> liftM (flip (PatTypeSig l) t) (fix p)
        PViewPat l e p          -> liftM2 (PViewPat l) (fix e) (fix p)
        _                       -> return p'
      where fix x = applyFixities fixs x
